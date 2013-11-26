unit uSftpCopyOutOperation;

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
  uNetworkFileSource,
  uNetworkFileSourceUtil,
  uNetworkCopyOutOperation,
  uSftpFileSource;

type
  { TSftpCopyOutOperation }

  TSftpCopyOutOperation = class(TNetworkCopyOutOperation)
  private
    FSftpFileSource: ISftpFileSource;
    FCurlFileSourceConnection: TSftpFileSourceConnection;
  protected
    function CopyFile(SourceFile: TFile; AbsoluteTargetFileName: String): Boolean; override;
  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;
    procedure Initialize; override;
  end;

implementation

uses
  libssh2, libssh2_sftp, uGlobs, uLog, uLng, uFileSourceOperationUI, DCOSUtils, uOSUtils;

{ TSftpCopyOutOperation }

function TSftpCopyOutOperation.CopyFile(SourceFile: TFile;
  AbsoluteTargetFileName: String): Boolean;
var
  SourceHandle: PLIBSSH2_SFTP_HANDLE;

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
    RemotePath:= StringReplace(SourceFile.FullPath, PathDelim, '/', [rfReplaceAll]);

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
            RaiseAbortOperation;
          fsourSkip: ; // Do nothing
          fsourSkipAll:
            FSkipOpenForReadingError := True;
        end;
      end;
    end;
  end;
  if not Assigned(SourceHandle) and (log_errors in gLogOptions) then
    logWrite(Thread, rsMsgLogError + rsMsgErrEOpen + ': ' + SourceFile.FullPath, lmtError, True);
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

      TargetFileStream:= OpenTarget(AbsoluteTargetFileName, TotalBytesToRead);
      if not Assigned(TargetFileStream) then
        Exit;

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
                DeleteFile := FSkipReadError and not (FMode in [fsohcmAppend, fsohcmResume]);
                if FSkipReadError then Exit;
                case AskQuestion(rsMsgErrERead + ' ' + SourceFile.FullPath + ':',
                                 E.Message,
                                 [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                                 fsourRetry, fsourSkip) of
                  fsourRetry:
                    bRetryRead := True;
                  fsourAbort:
                    RaiseAbortOperation;
                  fsourSkip:
                    Exit;
                  fsourSkipAll:
                    begin
                      DeleteFile := not (FMode in [fsohcmAppend, fsohcmResume]);
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
          mbDeleteFile(AbsoluteTargetFileName);
        end;
      end;
    end;
  end;
end;

constructor TSftpCopyOutOperation.Create(aSourceFileSource: IFileSource;
  aTargetFileSource: IFileSource; var theSourceFiles: TFiles;
  aTargetPath: String);
begin
  FSftpFileSource:= aSourceFileSource as ISftpFileSource;
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

procedure TSftpCopyOutOperation.Initialize;
begin
  inherited Initialize;
  FCurlFileSourceConnection:= TSftpFileSourceConnection(FConnection);
end;

end.

