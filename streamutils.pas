unit streamutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure SaveStringToStreamByte(Str: String; Stream: TStream);
function LoadStringFromStreamByte(Stream: TStream): String;
procedure SaveUTF8StringToStreamByte(Str: UTF8String; Stream: TStream);
function LoadUTF8StringFromStreamByte(Stream: TStream): UTF8String;
procedure SaveStringToStreamWord(Str: String; Stream: TStream);
function LoadStringFromStreamWord(Stream: TStream): String;
procedure SaveUTF8StringToStreamWord(Str: UTF8String; Stream: TStream);
function LoadUTF8StringFromStreamWord(Stream: TStream): UTF8String;
procedure SaveStringToStreamInt(Str: String; Stream: TStream);
function LoadStringFromStreamInt(Stream: TStream): String;
procedure SaveUTF8StringToStreamInt(Str: UTF8String; Stream: TStream);
function LoadUTF8StringFromStreamInt(Stream: TStream): UTF8String;
procedure SaveStringToStream(Str: String; Stream: TStream);
function LoadStringFromStream(Stream: TStream): String;
procedure SaveUTF8StringToStream(Str: UTF8String; Stream: TStream);
function LoadUTF8StringFromStream(Stream: TStream): UTF8String;

procedure SaveStringsToStream(Strings: TStrings; Stream: TStream);
procedure LoadStringsFromStream(Strings: TStrings; Stream: TStream);


implementation

uses
  LazUTF8;

procedure SaveStringToStreamByte(Str: String; Stream: TStream);
var
  L: Byte;
begin
  L := Length(Str);
  Stream.Write(L,SizeOf(Byte));
  Stream.Write(PChar(Str)^,L);
end;

function LoadStringFromStreamByte(Stream: TStream): String;
var
  L: Byte;
begin
  Stream.Read(L,SizeOf(Byte));
  SetLength(Result,L);
  if L > 0 then
    Stream.Read(PChar(Result)^,L);
end;

procedure SaveUTF8StringToStreamByte(Str: UTF8String; Stream: TStream);
var
  L: Byte;
begin
  L := UTF8Length(Str);
  Stream.Write(L,SizeOf(Byte));
  Stream.Write(PChar(Str)^,L);
end;

function LoadUTF8StringFromStreamByte(Stream: TStream): UTF8String;
var
  S: UTF8String;
  L: Byte;
begin
  Stream.Read(L,SizeOf(Byte));
  SetLength(S,L);
  if L > 0 then
    Stream.Read(PChar(S)^,L);
  Result := S;
end;

procedure SaveStringToStreamWord(Str: String; Stream: TStream);
var
  L: Word;
begin
  L := Length(Str);
  Stream.Write(L,SizeOf(Word));
  Stream.Write(PChar(Str)^,L);
end;

function LoadStringFromStreamWord(Stream: TStream): String;
var
  S: String;
  L: Word;
begin
  Stream.Read(L,SizeOf(Word));
  SetLength(S,L);
  if L > 0 then
    Stream.Read(PChar(S)^,L);
  Result := S;
end;

procedure SaveUTF8StringToStreamWord(Str: UTF8String; Stream: TStream);
var
  L: Word;
begin
  L := UTF8Length(Str);
  Stream.Write(L,SizeOf(Word));
  Stream.Write(PChar(Str)^,L);
end;

function LoadUTF8StringFromStreamWord(Stream: TStream): UTF8String;
var
  S: UTF8String;
  L: Word;
begin
  Stream.Read(L,SizeOf(Word));
  SetLength(S,L);
  if L > 0 then
    Stream.Read(PChar(S)^,L);
  Result := S;
end;

procedure SaveStringToStreamInt(Str: String; Stream: TStream);
var
  L: Integer;
begin
  L := Length(Str);
  Stream.Write(L,SizeOf(Integer));
  Stream.Write(PChar(Str)^,L);
end;

function LoadStringFromStreamInt(Stream: TStream): String;
var
  L: Integer;
begin
  Stream.Read(L,SizeOf(Integer));
  SetLength(Result,L);
  if L > 0 then
    Stream.Read(PChar(Result)^,L);
end;

procedure SaveUTF8StringToStreamInt(Str: UTF8String; Stream: TStream);
var
  L: Integer;
begin
  L := UTF8Length(Str);
  Stream.Write(L,SizeOf(Integer));
  Stream.Write(PChar(Str)^,L);
end;

function LoadUTF8StringFromStreamInt(Stream: TStream): UTF8String;
var
  S: UTF8String;
  L: Integer;
begin
  Stream.Read(L,SizeOf(Integer));
  SetLength(S,L);
  if L > 0 then
    Stream.Read(PChar(S)^,L);
  Result := S;
end;

procedure SaveStringToStream(Str: String; Stream: TStream);
begin
  SaveStringToStreamWord(Str,Stream);
end;

function LoadStringFromStream(Stream: TStream): String;
begin
  Result := LoadStringFromStreamWord(Stream);
end;

procedure SaveUTF8StringToStream(Str: UTF8String; Stream: TStream);
begin
  SaveUTF8StringToStreamWord(Str,Stream);
end;

function LoadUTF8StringFromStream(Stream: TStream): UTF8String;
begin
  Result := LoadUTF8StringFromStreamWord(Stream);
end;

procedure SaveStringsToStream(Strings: TStrings; Stream: TStream);
var
  I,J: Word;
  S: String;
begin
  J := Strings.Count;
  Stream.Write(J,SizeOf(J));
  if J > 0 then
    for I := 0 to J - 1 do
      begin
        S := Strings[I];
        SaveStringToStream(S,Stream);
      end;
end;

procedure LoadStringsFromStream(Strings: TStrings; Stream: TStream);
var
  I,J: Word;
  S: String;
begin
  Strings.Clear;
  Stream.Read(J,SizeOf(J));
  if J > 0 then
    for I := 0 to J - 1 do
      begin
        S := LoadStringFromStream(Stream);
        Strings.Add(S);
      end;
end;

end.

