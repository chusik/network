unit uSftpFilleSourceUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFile, uFileSystemUtil, uSftpFileSource,
  uFileSourceOperationUI, uFileSourceOperation, uFileSourceCopyOperation,
  uNetworkFileSourceUtil;

type

  { TSftpTreeBuilder }

  TSftpTreeBuilder = class(TFileSystemTreeBuilder)
    private
      FConnection: TSftpFileSourceConnection;
    protected
      procedure AddFilesInDirectory(srcPath: String; CurrentNode: TFileTreeNode); override;
      procedure AddLinkTarget(aFile: TFile; CurrentNode: TFileTreeNode); override;
    public
      constructor Create(Connection: TSftpFileSourceConnection;
                         AskQuestionFunction: TAskQuestionFunction;
                         CheckOperationStateFunction: TCheckOperationStateFunction);
  end;

  { TSftpOperationHelper }

  TSftpOperationHelper = class(TNetworkOperationHelper)
  private
    FCurlFileSourceConnection: TSftpFileSourceConnection;
    function CopyFile(SourceFile: TFile; TargetFileName: String; Mode: TFileSystemOperationHelperCopyMode): Boolean; override;
  public
    constructor Create(Connection: TSftpFileSourceConnection;
                       AskQuestionFunction: TAskQuestionFunction;
                       AbortOperationFunction: TAbortOperationFunction;
                       CheckOperationStateFunction: TCheckOperationStateFunction;
                       UpdateStatisticsFunction: TUpdateStatisticsFunction;
                       OperationThread: TThread;
                       Mode: TFileSystemOperationHelperMode;
                       TargetPath: String;
                       StartingStatistics: TFileSourceCopyOperationStatistics);
  end;

implementation

uses
  libssh,  DCClassesUtf8, uLng, uGlobs, uLOg, DCOSUtils;

{ TSftpOperationHelper }

function TSftpOperationHelper.CopyFile(SourceFile: TFile;
  TargetFileName: String; Mode: TFileSystemOperationHelperCopyMode): Boolean;
var
  SourceHandle: PLIBSSH2_SFTP_HANDLE;
              // Mode: TFileSystemOperationHelperCopyMode;
  TargetFileStream: TFileStreamEx;
  iTotalDiskSize, iFreeDiskSize: Int64;
  bRetryRead, bRetryWrite: Boolean;
  BytesRead, BytesToRead, BytesWrittenTry, BytesWritten: Int64;
  TotalBytesToRead: Int64 = 0;
  NewPos: Int64;
  DeleteFile: Boolean = False;

procedure OpenSourceFile;
var
  bRetry: Boolean = True;
  RemotePath: UTF8String;
begin
  while bRetry do
  begin
    bRetry := False;
    RemotePath:= CreateNetworkPath(SourceFile.FullPath);

    repeat
      SourceHandle:= libssh2_sftp_open(FCurlFileSourceConnection.Session_,
                                       PAnsiChar(RemotePath),
                                       LIBSSH2_FXF_READ, 0);
      WriteLn(libssh2_session_last_errno(FCurlFileSourceConnection.ss));
      WriteLn(libssh2_sftp_last_error(FCurlFileSourceConnection.Session_));
    until not ((SourceHandle = nil) and (libssh2_session_last_errno(FCurlFileSourceConnection.ss) = LIBSSH2_ERROR_EAGAIN));

    if (SourceHandle <> nil) then
      TotalBytesToRead := SourceFile.Size
    else
    begin
      if not FSkipOpenForReadingError then
      begin
        case AskQuestion(rsMsgErrEOpen + ': ' + SourceFile.FullPath, '',
                         [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                         fsourRetry, fsourSkip) of
          fsourRetry:
            bRetry := True;
          fsourAbort:
            AbortOperation;
          fsourSkip: ; // Do nothing
          fsourSkipAll:
            FSkipOpenForReadingError := True;
        end;
      end;
    end;
  end;
  if not Assigned(SourceHandle) and (log_errors in gLogOptions) then
    logWrite(FOperationThread, rsMsgLogError + rsMsgErrEOpen + ': ' + SourceFile.FullPath, lmtError, True);
end;

begin
  Result:= False;
  BytesToRead := 4096;//FBufferSize;
  SourceHandle := nil;
  TargetFileStream := nil; // for safety exception handling
  try
    try
      OpenSourceFile;
      if not Assigned(SourceHandle) then
        Exit;

      TargetFileStream:= OpenTarget(TargetFileName, TotalBytesToRead);
      if not Assigned(TargetFileStream) then
        Exit;

      if Mode = fsohcmResume then
      begin
        NewPos:= (SourceFile.Size - TotalBytesToRead);
        libssh2_sftp_seek64(SourceHandle, NewPos);
      end;

      while TotalBytesToRead > 0 do
      begin
        // Without the following line the reading is very slow
        // if it tries to read past end of file.
        if TotalBytesToRead < BytesToRead then
          BytesToRead := TotalBytesToRead;

        repeat
          try
            bRetryRead := False;
            BytesRead := libssh2_sftp_read(SourceHandle, PAnsiChar(FBuffer), BytesToRead);

            if (BytesRead < 0) then
              Raise EReadError.Create(IntToStr(BytesRead));

            TotalBytesToRead := TotalBytesToRead - BytesRead;

            if not WriteTarget(TargetFileStream, FBuffer, BytesRead, DeleteFile) then
              Exit;
          except
            on E: EReadError do
              begin
                DeleteFile := FSkipReadError and not (Mode in [fsohcmAppend, fsohcmResume]);
                if FSkipReadError then Exit;
                case AskQuestion(rsMsgErrERead + ' ' + SourceFile.FullPath + ':',
                                 E.Message,
                                 [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                                 fsourRetry, fsourSkip) of
                  fsourRetry:
                    bRetryRead := True;
                  fsourAbort:
                    AbortOperation;
                  fsourSkip:
                    Exit;
                  fsourSkipAll:
                    begin
                      DeleteFile := not (Mode in [fsohcmAppend, fsohcmResume]);
                      FSkipReadError := True;
                      Exit;
                    end;
                end; // case
              end;
          end;
        until not bRetryRead;

        with FStatistics do
        begin
          CurrentFileDoneBytes := CurrentFileDoneBytes + BytesRead;
          DoneBytes := DoneBytes + BytesRead;

          UpdateStatistics(FStatistics);
        end;

        CheckOperationState; // check pause and stop
      end;//while

      Result:= True;

    except
      on EFileSourceOperationAborting do
      begin
        // Always delete file when user aborted operation.
        DeleteFile := True;
        raise;
      end;
    end;

  finally
    libssh2_sftp_close_handle(SourceHandle);
    if Assigned(TargetFileStream) then
    begin
      FreeAndNil(TargetFileStream);
      if TotalBytesToRead > 0 then
      begin
        // There was some error, because not all of the file has been copied.
        // Ask if delete the not completed target file.
        if DeleteFile or
           (AskQuestion('', rsMsgDeletePartiallyCopied,
                        [fsourYes, fsourNo], fsourYes, fsourNo) = fsourYes) then
        begin
          mbDeleteFile(TargetFileName);
        end;
      end;
    end;
  end;
end;

constructor TSftpOperationHelper.Create(Connection: TSftpFileSourceConnection;
  AskQuestionFunction: TAskQuestionFunction;
  AbortOperationFunction: TAbortOperationFunction;
  CheckOperationStateFunction: TCheckOperationStateFunction;
  UpdateStatisticsFunction: TUpdateStatisticsFunction;
  OperationThread: TThread; Mode: TFileSystemOperationHelperMode;
  TargetPath: String; StartingStatistics: TFileSourceCopyOperationStatistics);
begin
  FCurlFileSourceConnection:= Connection;
  inherited Create(AskQuestionFunction, AbortOperationFunction, CheckOperationStateFunction,
                   UpdateStatisticsFunction, OperationThread, Mode,
                   TargetPath, StartingStatistics);
end;

{ TSftpTreeBuilder }

procedure TSftpTreeBuilder.AddFilesInDirectory(srcPath: String;
  CurrentNode: TFileTreeNode);
var
  aFile: TFile;
  Return: Integer;
  RemotePath: UTF8String;
  Handle: PLIBSSH2_SFTP_HANDLE;
  aFileName: array[0..1023] of AnsiChar;
  aFullData: array[0..1023] of AnsiChar;
  Attributes: LIBSSH2_SFTP_ATTRIBUTES;
begin
  with FConnection do
  begin
    RemotePath:= CreateNetworkPath(srcPath);
    Handle := libssh2_sftp_opendir(Session_, PAnsiChar(RemotePath));

    repeat
      Return:= libssh2_sftp_readdir_ex(Handle, aFileName, SizeOf(aFileName),
                                       aFullData, SizeOf(aFullData), @Attributes);
      if (Return > 0) then
      begin
        if (aFileName = '.') or (aFileName = '..') then Continue;

        aFile:= TSftpFileSource.CreateFile(FConnection, srcPath, aFileName, @Attributes);

        AddItem(aFile, CurrentNode);
      end;
    until (Return <= 0);
    libssh2_sftp_closedir(Handle);
  end;
end;

procedure TSftpTreeBuilder.AddLinkTarget(aFile: TFile;
  CurrentNode: TFileTreeNode);
begin

end;

constructor TSftpTreeBuilder.Create(Connection: TSftpFileSourceConnection;
  AskQuestionFunction: TAskQuestionFunction;
  CheckOperationStateFunction: TCheckOperationStateFunction);
begin
  FConnection:= Connection;
  inherited Create(AskQuestionFunction, CheckOperationStateFunction);
end;

end.

