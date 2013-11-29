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
  uNetworkFileSourceUtil,
  uFileSystemUtil;

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
    FRenamingFiles,
    FRenamingRootDir: Boolean;
    FRenameNameMask, FRenameExtMask: String;
    FFileExistsOption: TFileSourceOperationOptionFileExists;
    FDirExistsOption: TFileSourceOperationOptionDirectoryExists;
  private
    function ProcessFile(aFile: TFile; AbsoluteTargetFileName: String): Boolean;
    function ProcessDirectory(aFile: TFile; AbsoluteTargetFileName: String): Boolean;
    function TargetExists(aFile: TFile; AbsoluteTargetFileName: String): TFileSystemOperationTargetExistsResult;
    function DirExists(aFile: TFile;
                       AbsoluteTargetFileName: String;
                       AllowCopyInto: Boolean): TFileSourceOperationOptionDirectoryExists;
    function FileExists(aFile: TFile;
                        AbsoluteTargetFileName: String;
                        AllowAppend: Boolean): TFileSourceOperationOptionFileExists;
  protected
    FBuffer: PByte;
    FMode: TFileSystemOperationHelperCopyMode;
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
  uFileSourceOperationUI, uLng, uOSUtils, DCOSUtils, uTypes, DCBasicTypes,
  uFileProcs, DCDateTimeUtils, DCStrUtils;

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
  case TargetExists(aFile, AbsoluteTargetFileName) of
    fsoterSkip:
      begin
        Result := False;
        //CountStatistics(aNode);
      end;

    fsoterDeleted, fsoterNotExists:
      begin
        begin
          // Create target directory.
          if mbCreateDir(AbsoluteTargetFileName) then
          begin
            Result:= True;
            // Copy/Move all files inside.
            // Result := ProcessNode(aNode, IncludeTrailingPathDelimiter(AbsoluteTargetFileName));
            // Copy attributes after copy/move directory contents, because this operation can change date/time
            // CopyProperties(aNode.TheFile.FullPath, AbsoluteTargetFileName);
          end
          else
          begin
            // Error - all files inside not copied/moved.
            // ShowError(rsMsgLogError + Format(rsMsgErrForceDir, [AbsoluteTargetFileName]));
            Result := False;
            // CountStatistics(aNode);
          end;
        end;
      end;

    fsoterAddToTarget:
      begin
        // Don't create existing directory, but copy files into it.
        Result := True;
      end;

    else
      raise Exception.Create('Invalid TargetExists result');
  end;
end;

function TNetworkCopyOutOperation.TargetExists(aFile: TFile;
  AbsoluteTargetFileName: String): TFileSystemOperationTargetExistsResult;
var
  Attrs, LinkTargetAttrs: TFileAttrs;
  SourceFile: TFile;

  function DoDirectoryExists(AllowCopyInto: Boolean): TFileSystemOperationTargetExistsResult;
  begin
    case DirExists(SourceFile, AbsoluteTargetFileName, AllowCopyInto) of
      fsoodeSkip:
        Exit(fsoterSkip);
      fsoodeDelete:
        begin
          if FPS_ISLNK(Attrs) then
            mbDeleteFile(AbsoluteTargetFileName)
          else
            DelTree(AbsoluteTargetFileName);
          Exit(fsoterDeleted);
        end;
      fsoodeCopyInto:
        begin
          Exit(fsoterAddToTarget);
        end;
      else
        raise Exception.Create('Invalid dir exists option');
    end;
  end;

  function DoFileExists(AllowAppend: Boolean): TFileSystemOperationTargetExistsResult;
  begin
    case FileExists(SourceFile, AbsoluteTargetFileName, AllowAppend) of
      fsoofeSkip:
        Exit(fsoterSkip);
      fsoofeOverwrite:
        begin
          mbDeleteFile(AbsoluteTargetFileName);
          Exit(fsoterDeleted);
        end;
      fsoofeAppend:
        begin
          Exit(fsoterAddToTarget);
        end;
      fsoofeResume:
        begin
          Exit(fsoterResume);
        end;
      else
        raise Exception.Create('Invalid file exists option');
    end;
  end;

  function IsLinkFollowed: Boolean;
  begin
    // If link was followed then it's target is stored in a subnode.
    Result := SourceFile.AttributesProperty.IsLink;// and (aNode.SubNodesCount > 0);
  end;

  function AllowAppendFile: Boolean;
  begin
    Result := (not SourceFile.AttributesProperty.IsDirectory) and (not FReserveSpace) and
              ((not SourceFile.AttributesProperty.IsLink) or
               (IsLinkFollowed and (not aFile.AttributesProperty.IsDirectory)));
  end;

  function AllowCopyInto: Boolean;
  begin
    Result := SourceFile.AttributesProperty.IsDirectory or
              (IsLinkFollowed and aFile.IsDirectory);
  end;

begin
  Attrs := mbFileGetAttr(AbsoluteTargetFileName);
  if Attrs <> faInvalidAttributes then
  begin
    SourceFile := aFile;

    // Target exists - ask user what to do.
    if FPS_ISDIR(Attrs) then
    begin
      Result := DoDirectoryExists(AllowCopyInto)
    end
    else if FPS_ISLNK(Attrs) then
    begin
      // Check if target of the link exists.
      LinkTargetAttrs := mbFileGetAttrNoLinks(AbsoluteTargetFileName);
      if (LinkTargetAttrs <> faInvalidAttributes) then
      begin
        if FPS_ISDIR(LinkTargetAttrs) then
          Result := DoDirectoryExists(AllowCopyInto)
        else
          Result := DoFileExists(AllowAppendFile);
      end
      else
        // Target of link doesn't exist. Treat link as file and don't allow append.
        Result := DoFileExists(False);
    end
    else
      // Existing target is a file.
      Result := DoFileExists(AllowAppendFile);
  end
  else
    Result := fsoterNotExists;
end;

function TNetworkCopyOutOperation.DirExists(aFile: TFile;
  AbsoluteTargetFileName: String; AllowCopyInto: Boolean
  ): TFileSourceOperationOptionDirectoryExists;
var
  PossibleResponses: array of TFileSourceOperationUIResponse = nil;
  DefaultOkResponse: TFileSourceOperationUIResponse;

  procedure AddResponse(Response: TFileSourceOperationUIResponse);
  begin
    SetLength(PossibleResponses, Length(PossibleResponses) + 1);
    PossibleResponses[Length(PossibleResponses) - 1] := Response;
  end;

begin
  case FDirExistsOption of
    fsoodeNone:
      begin
        if AllowCopyInto then
        begin
          AddResponse(fsourCopyInto);
          AddResponse(fsourCopyIntoAll);
        end;
        AddResponse(fsourSkip);
        AddResponse(fsourSkipAll);
        AddResponse(fsourCancel);

        if AllowCopyInto then
          DefaultOkResponse := fsourCopyInto
        else
          DefaultOkResponse := fsourSkip;

        case AskQuestion(Format(rsMsgFolderExistsRwrt, [AbsoluteTargetFileName]), '',
                         PossibleResponses, DefaultOkResponse, fsourSkip) of
          fsourOverwrite:
            Result := fsoodeDelete;
          fsourCopyInto:
            Result := fsoodeCopyInto;
          fsourCopyIntoAll:
            begin
              FDirExistsOption := fsoodeCopyInto;
              Result := fsoodeCopyInto;
            end;
          fsourSkip:
            Result := fsoodeSkip;
          fsourOverwriteAll:
            begin
              FDirExistsOption := fsoodeDelete;
              Result := fsoodeDelete;
            end;
          fsourSkipAll:
            begin
              FDirExistsOption := fsoodeSkip;
              Result := fsoodeSkip;
            end;
          fsourNone,
          fsourCancel:
            RaiseAbortOperation;
        end;
      end;

    else
      Result := FDirExistsOption;
  end;
end;

function TNetworkCopyOutOperation.FileExists(aFile: TFile;
  AbsoluteTargetFileName: String; AllowAppend: Boolean
  ): TFileSourceOperationOptionFileExists;
const
  Responses: array[0..9] of TFileSourceOperationUIResponse
    = (fsourOverwrite, fsourSkip, fsourAppend, fsourOverwriteAll,
       fsourSkipAll, fsourResume, fsourOverwriteOlder, fsourCancel,
       fsourOverwriteSmaller, fsourOverwriteLarger);
  ResponsesNoAppend: array[0..5] of TFileSourceOperationUIResponse
    = (fsourOverwrite, fsourSkip, fsourOverwriteAll, fsourSkipAll,
       fsourOverwriteOlder, fsourCancel);
var
  Message: String;
  PossibleResponses: array of TFileSourceOperationUIResponse;

  function OverwriteOlder: TFileSourceOperationOptionFileExists;
  begin
    if aFile.ModificationTime > FileTimeToDateTime(mbFileAge(AbsoluteTargetFileName)) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

  function OverwriteSmaller: TFileSourceOperationOptionFileExists;
  begin
    if aFile.Size > mbFileSize(AbsoluteTargetFileName) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

  function OverwriteLarger: TFileSourceOperationOptionFileExists;
  begin
    if aFile.Size < mbFileSize(AbsoluteTargetFileName) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

begin
  case FFileExistsOption of
    fsoofeNone:
      begin
        case AllowAppend of
          True :  PossibleResponses := Responses;
          False:  PossibleResponses := ResponsesNoAppend;
        end;
        Message:= FileExistsMessage(AbsoluteTargetFileName, aFile.FullPath,
                                    aFile.Size, aFile.ModificationTime);
        case AskQuestion(Message, '',
                         PossibleResponses, fsourOverwrite, fsourSkip) of
          fsourOverwrite:
            Result := fsoofeOverwrite;
          fsourSkip:
            Result := fsoofeSkip;
          fsourAppend:
            begin
              //FFileExistsOption := fsoofeAppend; - for AppendAll
              Result := fsoofeAppend;
            end;
          fsourResume:
            begin
              Result := fsoofeResume;
            end;
          fsourOverwriteAll:
            begin
              FFileExistsOption := fsoofeOverwrite;
              Result := fsoofeOverwrite;
            end;
          fsourSkipAll:
            begin
              FFileExistsOption := fsoofeSkip;
              Result := fsoofeSkip;
            end;
          fsourOverwriteOlder:
            begin
              FFileExistsOption := fsoofeOverwriteOlder;
              Result:= OverwriteOlder;
            end;
          fsourOverwriteSmaller:
            begin
              FFileExistsOption := fsoofeOverwriteSmaller;
              Result:= OverwriteSmaller;
            end;
          fsourOverwriteLarger:
            begin
              FFileExistsOption := fsoofeOverwriteLarger;
              Result:= OverwriteLarger;
            end;
          fsourNone,
          fsourCancel:
            RaiseAbortOperation;
        end;
      end;
    fsoofeOverwriteOlder:
      begin
        Result:= OverwriteOlder;
      end;
    fsoofeOverwriteSmaller:
      begin
        Result:= OverwriteSmaller;
      end;
    fsoofeOverwriteLarger:
      begin
        Result:= OverwriteLarger;
      end;

    else
      Result := FFileExistsOption;
  end;
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
  FRenamingFiles := False;
  FRenamingRootDir := False;
  FFileExistsOption := fsoofeNone;

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
  // Split file mask into name and extension
  SplitFileMask(RenameMask, FRenameNameMask, FRenameExtMask);

  // Create destination path if it doesn't exist.
  if not mbDirectoryExists(TargetPath) then
    if not mbForceDirectory(TargetPath) then
      Exit; // do error
end;

procedure TNetworkCopyOutOperation.MainExecute;
var
  aFile: TFile;
  OldDoneBytes: Int64;
  ProcessResult: Boolean;
  CurrentFileIndex: Integer;
  AbsoluteTargetFileName: String;
begin
  FRenamingFiles := (RenameMask <> '*.*') and (RenameMask <> '');

  // If there is a single root dir and rename mask doesn't have wildcards
  // treat is as a rename of the root dir.
  if (FFullFilesTreeToCopy.Count = 1) and FRenamingFiles then
  begin
    aFile := FFullFilesTreeToCopy[0];
    if (aFile.IsDirectory or aFile.IsLinkToDirectory) and
       not ContainsWildcards(RenameMask) then
    begin
      FRenamingFiles := False;
      FRenamingRootDir := True;
    end;
  end;

  for CurrentFileIndex:= 0 to FFullFilesTreeToCopy.Count - 1 do
  begin
    // If there will be an error the DoneBytes value
    // will be inconsistent, so remember it here.
    OldDoneBytes := FStatistics.DoneBytes;

    aFile := FFullFilesTreeToCopy[CurrentFileIndex];
    // Filenames must be relative to the current directory.
    AbsoluteTargetFileName := TargetPath + ExtractDirLevel(FFullFilesTreeToCopy.Path, aFile.Path);

    if FRenamingRootDir then
      AbsoluteTargetFileName := AbsoluteTargetFileName + RenameMask
    else
      AbsoluteTargetFileName := AbsoluteTargetFileName + uFileSystemUtil.ApplyRenameMask(aFile, FRenameNameMask, FRenameExtMask);

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

