unit uNetworkFileSourceUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

//type
  //TNetworkOperationHelperCopyMode =
  //  (fsohcmDefault, fsohcmAppend, fsohcmResume);

function CreateNetworkPath(const Path: UTF8String): UTF8String;

implementation

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

end.

