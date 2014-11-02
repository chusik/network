unit uNetworkFileSourceUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCClassesUtf8, uFileSystemUtil,   uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI;

type

  { TNetworkOperationHelper }

  TNetworkOperationHelper = class(TFileSystemOperationHelper)
  public
    function OpenTarget(const TargetFileName: UTF8String; var TotalBytesToRead: Int64): TFileStreamEx;
    function WriteTarget(TargetFileStream: TFileStreamEx; Buffer: PByte; BytesRead: Int64; out DeleteFile: Boolean): Boolean;
  end;

function CreateNetworkPath(const Path: UTF8String): UTF8String;

implementation

uses
  uLng, uLog, uGlobs, uFileSourceOperationUI, uOSUtils, DCOSUtils;

function CreateNetworkPath(const Path: UTF8String): UTF8String;
var
  I: Integer;
begin
  Result:= Path;
  if PathDelim <> '/' then
  begin
    for I:= 1 to Length(Path) do
    begin
      if Result[I] = PathDelim then
        Result[I]:= '/';
    end;
  end;
end;

{ TNetworkOperationHelper }

function TNetworkOperationHelper.OpenTarget(const TargetFileName: UTF8String;
  var TotalBytesToRead: Int64): TFileStreamEx;
                          var Mode: TFileSystemOperationHelperCopyMode;
  function GetMsgByMode: String;
  begin
    {
    if FMode in [fsohcmAppend, fsohcmResume] then
      Result := rsMsgErrEOpen
    else}
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
          AbortOperation;
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
      case Mode of
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
    logWrite(FOperationThread, rsMsgLogError + GetMsgByMode + ': ' + TargetFileName, lmtError, True);
end;

function TNetworkOperationHelper.WriteTarget(TargetFileStream: TFileStreamEx;
  Buffer: PByte; BytesRead: Int64; out DeleteFile: Boolean): Boolean;
var
                             Mode: TFileSystemOperationHelperCopyMode;
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
                   AbortOperation;
                 fsourSkip:
                   Exit;
               end; // case
             end
           else
             begin
               DeleteFile := FSkipWriteError and not (Mode in [fsohcmAppend, fsohcmResume]);
               if FSkipWriteError then Exit;
               case AskQuestion(rsMsgErrEWrite + ' ' + TargetFileStream.FileName + ':',
                                E.Message,
                                [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                                fsourRetry, fsourSkip) of
                 fsourRetry:
                   bRetryWrite := True;
                 fsourAbort:
                   AbortOperation;
                 fsourSkip:
                   Exit;
                 fsourSkipAll:
                   begin
                     DeleteFile := not (Mode in [fsohcmAppend, fsohcmResume]);
                     FSkipWriteError := True;
                     Exit;
                   end;
               end; // case
             end;

         end; // on do
     end; // except
   until not bRetryWrite;
end;

end.

