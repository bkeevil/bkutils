unit ErrorMsg;

{$mode objfpc}{$H+}

interface

function GetErrorMessage(ErrorCode: Integer): String;

implementation

{$IFDEF Linux}
uses SysUtils, Errors;
{$ENDIF}

{$IFDEF Windows}
uses SysUtils, Windows;
{$ENDIF}

{$IFDEF Linux}
function GetErrorMessage(ErrorCode: Integer): String;
begin
  Result := StrError(ErrorCode);
end;
{$ENDIF}

{$IFDEF Windows}
function GetErrorMessage(ErrorCode: Integer): String;
var
  Msg: PChar;
begin
  Msg := StrAlloc(1024);
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS,nil,ErrorCode,0,Msg,1024,nil);
  Result := StrPas(Msg);
  StrDispose(Msg);
end;
{$ENDIF}

end.

