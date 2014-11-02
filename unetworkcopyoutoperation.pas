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
    FFullFilesTreeToCopy: TFileTree;  // source files including all files/dirs in subdirectories
    // Options
    FReserveSpace,
    FCheckFreeSpace: Boolean;
    FSkipAllBigFiles: Boolean;
    FRenamingFiles,
    FRenamingRootDir: Boolean;
    FRenameNameMask, FRenameExtMask: String;
    FOperationHelper: TFileSystemOperationHelper;
    //FFileExistsOption: TFileSourceOperationOptionFileExists;
    FDirExistsOption: TFileSourceOperationOptionDirectoryExists;
  private

  protected
    FBuffer: PByte;
    FSkipReadError: Boolean;
    FSkipWriteError: Boolean;
    FSkipOpenForReadingError: Boolean;
    FSkipOpenForWritingError: Boolean;
    FMode: TFileSystemOperationHelperCopyMode;
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
//    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
  protected
    function CreateFileSystemTreeBuilder: TFileSystemTreeBuilder; virtual; abstract;
    function CreateFileSystemOperationHelper(StartingStatistics: TFileSourceCopyOperationStatistics): TFileSystemOperationHelper; virtual; abstract;
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
{begin
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
end;}
var
  TreeBuilder: TFileSystemTreeBuilder;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  TreeBuilder := CreateFileSystemTreeBuilder;
  try
    //TreeBuilder.SymLinkOption  := Self.SymLinkOption;
    //TreeBuilder.SearchTemplate := Self.SearchTemplate;
    //TreeBuilder.ExcludeEmptyTemplateDirectories := Self.ExcludeEmptyTemplateDirectories;

    TreeBuilder.BuildFromFiles(SourceFiles);
    FFullFilesTreeToCopy := TreeBuilder.ReleaseTree;
    FStatistics.TotalFiles := TreeBuilder.FilesCount;
    FStatistics.TotalBytes := TreeBuilder.FilesSize;
  finally
    FreeAndNil(TreeBuilder);
  end;

  if Assigned(FOperationHelper) then
    FreeAndNil(FOperationHelper);

  FOperationHelper := CreateFileSystemOperationHelper(FStatistics);

  FOperationHelper.RenameMask := RenameMask;
  FOperationHelper.ReserveSpace :=  FReserveSpace;
  //FOperationHelper.CheckFreeSpace := CheckFreeSpace;
//  FOperationHelper.CopyAttributesOptions := CopyAttributesOptions;
//  FOperationHelper.SkipAllBigFiles := SkipAllBigFiles;
//  FOperationHelper.AutoRenameItSelf := AutoRenameItSelf;
//  FOperationHelper.CorrectSymLinks := CorrectSymLinks;
  FOperationHelper.FileExistsOption := FileExistsOption;
//  FOperationHelper.DirExistsOption := DirExistsOption;
//  FOperationHelper.SetPropertyError := SetPropertyError;

  FOperationHelper.Initialize;
end;

procedure TNetworkCopyOutOperation.MainExecute;
begin
  FOperationHelper.ProcessTree(FFullFilesTreeToCopy);
end;

procedure TNetworkCopyOutOperation.Finalize;
begin

end;

class function TNetworkCopyOutOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result := nil; //TWfxPluginCopyOutOperationOptionsUI;
end;

end.

