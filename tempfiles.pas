unit TempFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTempFileStream }

  TTempFileStream = class(TFileStream)
    public
      constructor Create;
      constructor Create(Rights: Cardinal);
      destructor Destroy; override;
  end;

var
  {$IFDEF Unix}    TempFileDir: String = '/tmp/';     {$ENDIF}
  {$IFDEF Windows} TempFileDir: String = 'C:\Temp\'; {$ENDIF}

implementation

function RandomFilename(Len: Byte): String;
var
  B: Byte;
begin
  Result := '';
  for B := 1 to Len do
    Result := Result + chr(Random(52) + 65);
end;

function TempFilename: String;
begin
  repeat
    Result := TempFileDir + RandomFilename(16);
  until not FileExists(Result);
end;

{ TTempFileStream }

constructor TTempFileStream.Create;
begin
  if not DirectoryExists(TempFileDir) then
    CreateDir(TempFileDir);
  inherited Create(TempFilename,fmCreate);
end;

constructor TTempFileStream.Create(Rights: Cardinal);
begin
  if not DirectoryExists(TempFileDir) then
    CreateDir(TempFileDir);
  inherited Create(TempFilename,fmCreate,Rights);
end;

destructor TTempFileStream.Destroy;
begin
  inherited Destroy;
  DeleteFile(Filename);
end;

end.

