unit uCurlCopyInOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DCClassesUtf8,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI,
  uFile,
  uCurlFileSource,
  libCurl;

type

  { TCurlCopyInOperation }

  TCurlCopyInOperation = class(TFileSourceCopyInOperation)

  private
    FCurlFileSource: ICurlFileSource;
    FCurlFileSourceConnection: TCurlFileSourceConnection;
    FFullFilesTreeToCopy: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    // Options
    FDeleteFile: Boolean;
    FSkipReadError: Boolean;
    FRenameNameMask, FRenameExtMask: String;
    FLogCaption: String;
    FRenamingFiles,
    FRenamingRootDir,
    FSkipOpenForReadingError: Boolean;
    FFileExistsOption: TFileSourceOperationOptionFileExists;

  protected
    FSourceFileStream: TFileStreamEx;

    function ProcessFile(aFile: TFile; AbsoluteTargetFileName: String): CURLcode;
    function ProcessDirectory(aFile: TFile; AbsoluteTargetFileName: String): CURLcode;

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
  {fCurlCopyMoveOperationOptions,} DCStrUtils, uFileSystemUtil, uLog, uLng, uGlobs,
  uFileSourceOperationUI, uCurlUtil;

(* This is a return code for the read callback that, when returned, will
   signal libcurl to pause sending data on the current transfer. *)
const
  CURL_READFUNC_PAUSE = $10000001;

const
  (* Callback function for seeking in the input stream *)
  CURLOPT_SEEKFUNCTION = 167;
  CURLOPT_SEEKDATA = 168;

// -- TCurlCopyInOperation ---------------------------------------------

function ReadFunction(buffer: Pchar; size: size_t; nitems: size_t; instream: pointer): size_t; cdecl;
var
  iTemp: Int64 = 0;
  bRetryRead: Boolean;
  OperationState: TFileSourceOperationState;
  CurlCopyInOperation: TCurlCopyInOperation absolute instream;
begin
  Result := 0;

  if CurlCopyInOperation.State = fsosStopping then  // Cancel operation
    Exit(CURL_READFUNC_ABORT);

  repeat
    try
      bRetryRead := False;
      if iTemp = 0 then Raise EReadError.Create('Hello');
      Result:= CurlCopyInOperation.FSourceFileStream.Read(buffer^, size * nitems);
    except
      on E: EReadError do
      with CurlCopyInOperation do
      begin
        FDeleteFile := FSkipReadError;
        if FSkipReadError then Exit(-1);
        curl_easy_pause(FCurlFileSourceConnection.CurlHandle, CURLPAUSE_SEND);
        try
          case AskQuestion(rsMsgErrERead + ' ' + FSourceFileStream.FileName + ':',
                           E.Message,
                           [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                           fsourRetry, fsourSkip) of
            fsourRetry:
              bRetryRead := True;
            fsourAbort:
              Exit(CURL_READFUNC_ABORT);
            fsourSkip:
              Exit(-1);
            fsourSkipAll:
              begin
                FDeleteFile := True;
                FSkipReadError := True;
                Exit(-1);
              end;
          end; // case
        finally
          iTemp:= -1;
          curl_easy_pause(FCurlFileSourceConnection.CurlHandle, CURLPAUSE_SEND_CONT);
        end;
      end;
    end;
  until not bRetryRead;

  with CurlCopyInOperation.FStatistics do
  begin
    CurrentFileDoneBytes := CurrentFileDoneBytes + Result;
    DoneBytes := DoneBytes + Result;

    CurlCopyInOperation.UpdateStatistics(CurlCopyInOperation.FStatistics);
  end;
  OperationState:= CurlCopyInOperation.State;
//  if OperationState = fsosPausing then
  CurlCopyInOperation.CheckOperationState; // check pause and stop
end;

function TCurlCopyInOperation.ProcessFile(aFile: TFile; AbsoluteTargetFileName: String): CURLcode;
var
  bRetry: Boolean = True;
begin
  while bRetry do
  begin
    bRetry := False;
    FSourceFileStream.Free; // In case stream was created but 'while' loop run again
    try
      FSourceFileStream := TFileStreamEx.Create(aFile.FullPath, fmOpenRead or fmShareDenyNone);
    except
      on EFOpenError do
      begin
        if not FSkipOpenForReadingError then
        begin
          case AskQuestion(rsMsgErrEOpen + ': ' + aFile.FullPath, '',
                           [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                           fsourRetry, fsourSkip) of
            fsourRetry:
              bRetry := True;
            fsourAbort:
              RaiseAbortOperation;
            fsourSkip: ; // Do nothing
            fsourSkipAll:
              FSkipOpenForReadingError := True;
          end;
        end;
      end;
    end;
  end;
  if not Assigned(FSourceFileStream) and (log_errors in gLogOptions) then
    logWrite(Thread, rsMsgLogError + rsMsgErrEOpen + ': ' + aFile.FullPath, lmtError, True)
  else
    with FCurlFileSourceConnection do
    begin
      curl_easy_setopt(CurlHandle, CURLOPT_URL, [PAnsiChar(AbsoluteTargetFileName)]);
      curl_easy_setopt(CurlHandle, CURLOPT_UPLOAD, [Pointer(True)]);

      Result:= curl_easy_perform(CurlHandle);

      curl_easy_setopt(CurlHandle, CURLOPT_UPLOAD, [Pointer(False)]);
    end;
end;

function ExtractFilePath(const FileName: string): string;
var
  i : longint;
begin
  i := Length(FileName);
  while (i > 0) and (FileName[i] <> '/') do
    Dec(i);
  if I > 0 then
    Result := Copy(FileName, 1, i)
  else
    Result:='';
end;

function ExtractFileName(const FileName: string): string;
var
  i : longint;
begin
  I := Length(FileName);
  while (I > 0) and (FileName[I] <> '/') do
    Dec(I);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function TCurlCopyInOperation.ProcessDirectory(aFile: TFile; AbsoluteTargetFileName: String): CURLcode;
begin
  Result:= CreateDirectory(FCurlFileSourceConnection.CurlHandle, ExtractFilePath(AbsoluteTargetFileName), ExtractFileName(AbsoluteTargetFileName));
end;

constructor TCurlCopyInOperation.Create(aSourceFileSource: IFileSource;
                                             aTargetFileSource: IFileSource;
                                             var theSourceFiles: TFiles;
                                             aTargetPath: String);
begin
  FCurlFileSource:= aTargetFileSource as ICurlFileSource;
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);

  FRenamingFiles := False;
  FRenamingRootDir := False;
end;

destructor TCurlCopyInOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TCurlCopyInOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FillAndCount(SourceFiles, False, False,
               FFullFilesTreeToCopy,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)

  SplitFileMask(RenameMask, FRenameNameMask, FRenameExtMask);
  FCurlFileSourceConnection:= TCurlFileSourceConnection(FConnection);
end;

procedure TCurlCopyInOperation.MainExecute;
var
  I: Integer;
  cResult: CURLcode;
  sTargetFile : UTF8String;
  aFile: TFile;
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

  with FCurlFileSourceConnection do
  begin
    curl_easy_setopt(CurlHandle, CURLOPT_READFUNCTION, [@ReadFunction]);
    curl_easy_setopt(CurlHandle, CURLOPT_READDATA, [Pointer(Self)]);

  for I:= 0 to FFullFilesTreeToCopy.Count - 1 do
  with FCurlFileSourceConnection do
  begin
    aFile:= FFullFilesTreeToCopy.Items[I];

    // Filenames must be relative to the current directory.
    sTargetFile := TargetPath + ExtractDirLevel(FFullFilesTreeToCopy.Path, aFile.Path);

    if FRenamingRootDir then
      sTargetFile := sTargetFile + RenameMask
    else
      sTargetFile := sTargetFile + ApplyRenameMask(aFile, FRenameNameMask, FRenameExtMask);

    Delete(sTargetFile,1,1);
    sTargetFile:= StringReplace(sTargetFile,'\','/', [rfREplaceAll]);

    with FStatistics do
    begin
      CurrentFileFrom := aFile.FullPath;
      CurrentFileTo := sTargetFile;
      CurrentFileTotalBytes := aFile.Size;
      CurrentFileDoneBytes := 0;
    end;

    UpdateStatistics(FStatistics);

    if not aFile.IsDirectory then
      cResult := ProcessFile(aFile, sTargetFile)
    else
      cResult := ProcessDirectory(aFile, sTargetFile);


  end;
  WriteLn(curl_easy_strerror(cResult));

  end;
end;

procedure TCurlCopyInOperation.Finalize;
begin

end;

class function TCurlCopyInOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result := nil; //TCurlCopyInOperationOptionsUI;
end;

end.

