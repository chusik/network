unit uNetworkCopyOutOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DCClassesUtf8,
  uLog, uGlobs,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI,
  uFile,
  uNetworkFileSource,
  uNetworkFileSourceUtil;

type

  { TNetworkCopyOutOperation }

  TNetworkCopyOutOperation = class(TFileSourceCopyOutOperation)

  private
    FNetworkFileSource: INetworkFileSource;
    FFullFilesTreeToCopy: TFiles;  // source files including all files/dirs in subdirectories
    // Options
    FReserveSpace,
    FCheckFreeSpace: Boolean;
    FSkipAllBigFiles: Boolean;
    FFileExistsOption: TFileSourceOperationOptionFileExists;
  private
    function ProcessFile(aFile: TFile; AbsoluteTargetFileName: String): Boolean;
    function ProcessDirectory(aFile: TFile; AbsoluteTargetFileName: String): Boolean;
  protected
    FBuffer: PByte;
    FMode: TNetworkOperationHelperCopyMode;
    FSkipReadError: Boolean;
    FSkipWriteError: Boolean;
    FSkipOpenForReadingError: Boolean;
    FSkipOpenForWritingError: Boolean;
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
  protected
    function OpenTarget(const TargetFileName: UTF8String; var TotalBytesToRead: Int64): TFileStreamEx;
    function WriteTarget(TargetFileStream: TFileStreamEx; Buffer: PByte; BytesRead: Int64; out DeleteFile: Boolean): Boolean;
    function CopyFile(aFile: TFile; AbsoluteTargetFileName: String): Boolean; virtual; abstract;
  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

    class function GetOptionsUIClass: TFileSourceOperationOptionsUIClass; override;

    property FileExistsOption: TFileSourceOperationOptionFileExists read FFileExistsOption write FFileExistsOption;
    property NeedsConnection: Boolean read FNeedsConnection write FNeedsConnection;

  end;

implementation

uses
  uFileSourceOperationUI, uLng, uOSUtils, DCOSUtils;

// -- TNetworkCopyOutOperation ---------------------------------------------

function TNetworkCopyOutOperation.ProcessFile(aFile: TFile;
  AbsoluteTargetFileName: String): Boolean;
var
  iTotalDiskSize, iFreeDiskSize: Int64;
begin
  { Check disk free space }
  if FCheckFreeSpace = True then
  begin
    GetDiskFreeSpace(ExtractFilePath(AbsoluteTargetFileName), iFreeDiskSize, iTotalDiskSize);
    if aFile.Size > iFreeDiskSize then
    begin
      if FSkipAllBigFiles = True then
      begin
        Exit;
      end
      else
      begin
        case AskQuestion('', rsMsgNoFreeSpaceCont,
                         [fsourYes, fsourAll, fsourNo, fsourSkip, fsourSkipAll],
                         fsourYes, fsourNo) of
          fsourNo:
            RaiseAbortOperation;

          fsourSkip:
            Exit;

          fsourAll:
            FCheckFreeSpace := False;

          fsourSkipAll:
            begin
              FSkipAllBigFiles := True;
              Exit;
            end;
        end;
      end;
    end;
  end;

  Result:= CopyFile(aFile, AbsoluteTargetFileName);
end;

function TNetworkCopyOutOperation.ProcessDirectory(aFile: TFile;
  AbsoluteTargetFileName: String): Boolean;
begin

end;

procedure TNetworkCopyOutOperation.LogMessage(sMessage: String;
  logOptions: TLogOptions; logMsgType: TLogMsgType);
begin
  case logMsgType of
    lmtError:
      if not (log_errors in gLogOptions) then Exit;
    lmtInfo:
      if not (log_info in gLogOptions) then Exit;
    lmtSuccess:
      if not (log_success in gLogOptions) then Exit;
  end;

  if logOptions <= gLogOptions then
  begin
    logWrite(Thread, sMessage, logMsgType);
  end;
end;

function TNetworkCopyOutOperation.OpenTarget(const TargetFileName: UTF8String;
  var TotalBytesToRead: Int64): TFileStreamEx;

  function GetMsgByMode: String;
  begin
    if FMode in [fsohcmAppend, fsohcmResume] then
      Result := rsMsgErrEOpen
    else
      Result := rsMsgErrECreate;
  end;

  function HandleError: Boolean;
  begin
    Result := False;
    if not FSkipOpenForWritingError then
    begin
      case AskQuestion(GetMsgByMode + ': ' + TargetFileName, '',
                       [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                       fsourRetry, fsourSkip) of
        fsourRetry:
          Result := True;
        fsourAbort:
          RaiseAbortOperation;
        fsourSkip: ; // Do nothing
        fsourSkipAll:
          FSkipOpenForWritingError := True;
      end;
    end;
  end;

var
  NewPos,
  ResumeAt: Int64;
  bRetry: Boolean = True;
begin
  Result:= nil;
  while bRetry do
  begin
    bRetry := False;
    try
      Result.Free; // In case stream was created but 'while' loop run again
      case FMode of
      fsohcmAppend:
        begin
          Result := TFileStreamEx.Create(TargetFileName, fmOpenReadWrite);
          Result.Seek(0, soFromEnd); // seek to end
        end;
      fsohcmResume:
        begin
          Result := TFileStreamEx.Create(TargetFileName, fmOpenReadWrite);
          ResumeAt := Result.Seek(0, soFromEnd);
          TotalBytesToRead := TotalBytesToRead - ResumeAt;
        end
      else
        begin
          Result := TFileStreamEx.Create(TargetFileName, fmCreate);
          if FReserveSpace then
          begin
            Result.Size:= TotalBytesToRead;
            Result.Seek(0, fsFromBeginning);
          end;
        end;
      end;
    except
      on EFOpenError do
        bRetry := HandleError;
      on EFCreateError do
        bRetry := HandleError;
    end;
  end;
  if not Assigned(Result) and (log_errors in gLogOptions) then
    logWrite(Thread, rsMsgLogError + GetMsgByMode + ': ' + TargetFileName, lmtError, True);
end;

function TNetworkCopyOutOperation.WriteTarget(TargetFileStream: TFileStreamEx;
  Buffer: PByte; BytesRead: Int64; out DeleteFile: Boolean): Boolean;
var
  bRetryWrite: Boolean;
  iTotalDiskSize, iFreeDiskSize: Int64;
  BytesWrittenTry, BytesWritten: Int64;
begin
  Result := False;
  BytesWritten := 0;
  repeat
     try
       bRetryWrite := False;
       BytesWrittenTry := TargetFileStream.Write((Buffer + BytesWritten)^, BytesRead);
       BytesRead := BytesRead - BytesWrittenTry;
       BytesWritten := BytesWritten + BytesWrittenTry;
       if BytesWrittenTry = 0 then
       begin
         Raise EWriteError.Create(mbSysErrorMessage(GetLastOSError));
       end
       else if BytesWritten < BytesRead then
       begin
         bRetryWrite := True;   // repeat and try to write the rest
       end;
       Result := True;
     except
       on E: EWriteError do
         begin
           { Check disk free space }
           GetDiskFreeSpace(ExtractFilePath(TargetFileStream.FileName), iFreeDiskSize, iTotalDiskSize);
           if BytesRead > iFreeDiskSize then
             begin
               case AskQuestion(rsMsgNoFreeSpaceRetry, '',
                                [fsourYes, fsourNo, fsourSkip],
                                fsourYes, fsourNo) of
                 fsourYes:
                   bRetryWrite := True;
                 fsourNo:
                   RaiseAbortOperation;
                 fsourSkip:
                   Exit;
               end; // case
             end
           else
             begin
               DeleteFile := FSkipWriteError and not (FMode in [fsohcmAppend, fsohcmResume]);
               if FSkipWriteError then Exit;
               case AskQuestion(rsMsgErrEWrite + ' ' + TargetFileStream.FileName + ':',
                                E.Message,
                                [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                                fsourRetry, fsourSkip) of
                 fsourRetry:
                   bRetryWrite := True;
                 fsourAbort:
                   RaiseAbortOperation;
                 fsourSkip:
                   Exit;
                 fsourSkipAll:
                   begin
                     DeleteFile := not (FMode in [fsohcmAppend, fsohcmResume]);
                     FSkipWriteError := True;
                     Exit;
                   end;
               end; // case
             end;

         end; // on do
     end; // except
   until not bRetryWrite;
end;

constructor TNetworkCopyOutOperation.Create(aSourceFileSource: IFileSource;
                                              aTargetFileSource: IFileSource;
                                              var theSourceFiles: TFiles;
                                              aTargetPath: String);
begin
  FBuffer:= GetMem(65536);
  FNetworkFileSource:= aSourceFileSource as INetworkFileSource;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TNetworkCopyOutOperation.Destroy;
begin
  inherited Destroy;
  FreeMem(FBuffer);
end;

procedure TNetworkCopyOutOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
  // Gets full list of files (recursive)
  FNetworkFileSource.FillAndCount(FConnection,
                                  SourceFiles, False,
                                  FFullFilesTreeToCopy,
                                  FStatistics.TotalFiles,
                                  FStatistics.TotalBytes);
end;

procedure TNetworkCopyOutOperation.MainExecute;
var
  aFile: TFile;
  OldDoneBytes: Int64;
  ProcessResult: Boolean;
  CurrentFileIndex: Integer;
  AbsoluteTargetFileName: String;
begin
  for CurrentFileIndex := FFullFilesTreeToCopy.Count - 1 downto 0 do
  begin
    // If there will be an error the DoneBytes value
    // will be inconsistent, so remember it here.
    OldDoneBytes := FStatistics.DoneBytes;

    aFile := FFullFilesTreeToCopy[CurrentFileIndex];
    AbsoluteTargetFileName:= TargetPath + aFile.Name;

    with FStatistics do
    begin
      CurrentFileFrom := aFile.FullPath;
      CurrentFileTo := AbsoluteTargetFileName;
      CurrentFileTotalBytes := aFile.Size;
      CurrentFileDoneBytes := 0;
    end;

    UpdateStatistics(FStatistics);

    if aFile.IsDirectory or aFile.IsLinkToDirectory then
      ProcessResult:= ProcessDirectory(aFile, AbsoluteTargetFileName)
    else
      ProcessResult:= ProcessFile(aFile, AbsoluteTargetFileName);

    if ProcessResult then
      begin
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogCopy,
                   [aFile.FullPath + ' -> ' + AbsoluteTargetFileName]),
                   [log_cp_mv_ln], lmtSuccess);
      end
    else
      begin
        LogMessage(Format(rsMsgLogError + rsMsgLogCopy,
                   [aFile.FullPath + ' -> ' + AbsoluteTargetFileName]),
                   [log_cp_mv_ln], lmtError);
      end;

    with FStatistics do
    begin
      DoneFiles := DoneFiles + 1;
      DoneBytes := OldDoneBytes + aFile.Size;

      UpdateStatistics(FStatistics);
    end;

    CheckOperationState;
  end;
end;

procedure TNetworkCopyOutOperation.Finalize;
begin

end;

class function TNetworkCopyOutOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result := nil; //TWfxPluginCopyOutOperationOptionsUI;
end;

end.

