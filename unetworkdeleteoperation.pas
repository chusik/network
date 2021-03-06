unit uNetworkDeleteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceDeleteOperation,
  uNetworkFileSource,
  uFileSource,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uGlobs, uLog;

type

  TNetworkDeleteOperation = class(TFileSourceDeleteOperation)

  private
    FNetworkFileSource: INetworkFileSource;
    FFullFilesTreeToDelete: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceDeleteOperationStatistics; // local copy of statistics

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FSkipErrors: Boolean;
    FDeleteReadOnly: TFileSourceOperationOptionGeneral;

  protected
    function ProcessFile(aFile: TFile): Boolean;
    function ShowError(sMessage: String): TFileSourceOperationUIResponse;
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToDelete: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  DCOSUtils, uLng;

constructor TNetworkDeleteOperation.Create(aTargetFileSource: IFileSource;
                                             var theFilesToDelete: TFiles);
begin
  FSymLinkOption := fsooslNone;
  FSkipErrors := False;
  FDeleteReadOnly := fsoogNone;
  FFullFilesTreeToDelete := nil;
  FNetworkFileSource:= aTargetFileSource as INetworkFileSource;

  inherited Create(aTargetFileSource, theFilesToDelete);
end;

destructor TNetworkDeleteOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TNetworkDeleteOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
  // Gets full list of files (recursive)
  FNetworkFileSource.FillAndCount(FConnection,
                                  FilesToDelete, True,
                                  FFullFilesTreeToDelete,
                                  FStatistics.TotalFiles,
                                  FStatistics.TotalBytes);
end;

procedure TNetworkDeleteOperation.MainExecute;
var
  aFile: TFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := FFullFilesTreeToDelete.Count - 1 downto 0 do
  begin
    aFile := FFullFilesTreeToDelete[CurrentFileIndex];

    FStatistics.CurrentFile := aFile.FullPath;
    UpdateStatistics(FStatistics);

    ProcessFile(aFile);

    with FStatistics do
    begin
      DoneFiles := DoneFiles + 1;
      DoneBytes := DoneBytes + aFile.Size;

      UpdateStatistics(FStatistics);
    end;

    CheckOperationState;
  end;
end;

procedure TNetworkDeleteOperation.Finalize;
begin

end;

function TNetworkDeleteOperation.ProcessFile(aFile: TFile): Boolean;
var
  FileName: String;
  bRetry: Boolean;
  sMessage, sQuestion: String;
  logOptions: TLogOptions;
begin
  Result := False;
  FileName := aFile.FullPath;

  if FileIsReadOnly(aFile.Attributes) then
  begin
    case FDeleteReadOnly of
      fsoogNone:
        case AskQuestion(Format(rsMsgFileReadOnly, [FileName]), '',
                         [fsourYes, fsourAll, fsourSkip, fsourSkipAll],
                         fsourYes, fsourSkip) of
          fsourAll:
            FDeleteReadOnly := fsoogYes;
          fsourSkip:
            Exit;
          fsourSkipAll:
            begin
              FDeleteReadOnly := fsoogNo;
              Exit;
            end;
        end;

       fsoogNo:
         Exit;
    end;
  end;

  repeat
    bRetry := False;

    //if FileIsReadOnly(aFile.Attributes) then
    //  mbFileSetReadOnly(FileName, False);

    with FNetworkFileSource do
    if aFile.IsDirectory then // directory
      begin
        Result := RemoveDirectory(FConnection, FileName);
      end
    else
      begin // files and other stuff
        Result := DeleteFile(FConnection, FileName);
      end;

    if Result then
    begin // success
      if aFile.IsDirectory then
      begin
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogRmDir, [FileName]), [log_dir_op, log_delete], lmtSuccess);
      end
      else
      begin
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogDelete, [FileName]), [log_delete], lmtSuccess);
      end;
    end
    else // error
    begin
      if aFile.IsDirectory then
      begin
        logOptions := [log_dir_op, log_delete];
        sMessage := Format(rsMsgLogError + rsMsgLogRmDir, [FileName]);
        sQuestion := Format(rsMsgNotDelete, [FileName]);
      end
      else
      begin
        logOptions := [log_delete];
        sMessage := Format(rsMsgLogError + rsMsgLogDelete, [FileName]);
        sQuestion := Format(rsMsgNotDelete, [FileName]);
      end;

      if gSkipFileOpError or (FSkipErrors = True) then
        LogMessage(sMessage, logOptions, lmtError)
      else
      begin
        case AskQuestion(sQuestion, '',
                         [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                         fsourRetry, fsourSkip) of
          fsourRetry:
            bRetry := True;
          fsourSkipAll:
            FSkipErrors := True;
          fsourAbort:
            RaiseAbortOperation;
        end;
      end;
    end;
  until bRetry = False;
end;

function TNetworkDeleteOperation.ShowError(sMessage: String): TFileSourceOperationUIResponse;
begin
  if gSkipFileOpError then
  begin
    logWrite(Thread, sMessage, lmtError, True);
    Result := fsourSkip;
  end
  else
  begin
    Result := AskQuestion(sMessage, '', [fsourSkip, fsourCancel], fsourSkip, fsourCancel);
    if Result = fsourCancel then
      RaiseAbortOperation;
  end;
end;

procedure TNetworkDeleteOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

end.


