unit uNetworkSetFilePropertyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSetFilePropertyOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFile,
  uFileProperty,
  uNetworkFileSource;

type

  TNetworkSetFilePropertyOperation = class(TFileSourceSetFilePropertyOperation)

  protected
    FNetworkFileSource: INetworkFileSource;
    FFullFilesTree: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceSetFilePropertyOperationStatistics; // local copy of statistics

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theTargetFiles: TFiles;
                       var theNewProperties: TFileProperties); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;

  end;

implementation

uses
  DCBasicTypes, DCStrUtils, WfxPlugin, uWfxPluginUtil, DCDateTimeUtils;

constructor TNetworkSetFilePropertyOperation.Create(aTargetFileSource: IFileSource;
                                                      var theTargetFiles: TFiles;
                                                      var theNewProperties: TFileProperties);
begin
  FFullFilesTree := nil;
  FNetworkFileSource:= aTargetFileSource as INetworkFileSource;

  inherited Create(aTargetFileSource, theTargetFiles, theNewProperties);
end;

destructor TNetworkSetFilePropertyOperation.Destroy;
begin
  inherited Destroy;

  if Recursive then
  begin
    if Assigned(FFullFilesTree) then
      FreeAndNil(FFullFilesTree);
  end;
end;

procedure TNetworkSetFilePropertyOperation.Initialize;
var
  TotalBytes: Int64;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  if not Recursive then
    begin
      FFullFilesTree := TargetFiles;
      FStatistics.TotalFiles:= FFullFilesTree.Count;
    end
  else
    begin
      // Gets full list of files (recursive)
      FNetworkFileSource.FillAndCount(FConnection,
                                      TargetFiles, True,
                                      FFullFilesTree,
                                      FStatistics.TotalFiles,
                                      TotalBytes);
    end;
end;

procedure TNetworkSetFilePropertyOperation.MainExecute;
var
  aFile: TFile;
  aTemplateFile: TFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := 0 to FFullFilesTree.Count - 1 do
  begin
    aFile := FFullFilesTree[CurrentFileIndex];

    FStatistics.CurrentFile := aFile.FullPath;
    UpdateStatistics(FStatistics);

    if Assigned(TemplateFiles) and (CurrentFileIndex < TemplateFiles.Count) then
      aTemplateFile := TemplateFiles[CurrentFileIndex]
    else
      aTemplateFile := nil;

    SetProperties(aFile, aTemplateFile);

    with FStatistics do
    begin
      DoneFiles := DoneFiles + 1;
      UpdateStatistics(FStatistics);
    end;

    CheckOperationState;
  end;
end;

end.

