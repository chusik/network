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
  uFileSystemUtil,
  uSftpFileSource;

type
  { TSftpCopyOutOperation }

  TSftpCopyOutOperation = class(TNetworkCopyOutOperation)
  private
    FSftpFileSource: ISftpFileSource;
    FCurlFileSourceConnection: TSftpFileSourceConnection;
  protected
    function CreateFileSystemTreeBuilder: TFileSystemTreeBuilder; override;
    function CreateFileSystemOperationHelper(StartingStatistics: TFileSourceCopyOperationStatistics): TFileSystemOperationHelper; override;
  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;
    procedure Initialize; override;
  end;

implementation

uses
  libssh, uGlobs, uLog, uLng, uFileSourceOperationUI, DCOSUtils, uOSUtils,
  uSftpFilleSourceUtil;

{ TSftpCopyOutOperation }

function TSftpCopyOutOperation.CreateFileSystemTreeBuilder: TFileSystemTreeBuilder;
begin
  Result:= TSftpTreeBuilder.Create(FConnection as TSftpFileSourceConnection,
                                   @AskQuestion,
                                   @CheckOperationState);
end;

function TSftpCopyOutOperation.CreateFileSystemOperationHelper(
  StartingStatistics: TFileSourceCopyOperationStatistics
  ): TFileSystemOperationHelper;
begin
  Result:= TSftpOperationHelper.Create(FCurlFileSourceConnection,
                                       @AskQuestion,
                        @RaiseAbortOperation,
                        @CheckOperationState,
                        @UpdateStatistics,
                        Thread,
                        fsohmCopy,
                        TargetPath,
                                       StartingStatistics);
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
  FCurlFileSourceConnection:= TSftpFileSourceConnection(FConnection);
  inherited Initialize;
end;

end.

