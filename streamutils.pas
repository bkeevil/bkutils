unit streamutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure SaveStringToStreamByte(Str: String; Stream: TStream);
function LoadStringFromStreamByte(Stream: TStream): String;
procedure SaveStringToStreamWord(Str: String; Stream: TStream);
function LoadStringFromStreamWord(Stream: TStream): String;
procedure SaveStringToStreamInt(Str: String; Stream: TStream);
function LoadStringFromStreamInt(Stream: TStream): String;
procedure SaveStringToStream(Str: String; Stream: TStream);
function LoadStringFromStream(Stream: TStream): String;
procedure SaveStringsToStream(Strings: TStrings; Stream: TStream);
procedure LoadStringsFromStream(Strings: TStrings; Stream: TStream);
procedure EncryptStream(InStream, OutStream: TStream; Password: String);
procedure DecryptStream(InStream, OutStream: TStream; Password: String);


implementation

uses
  Crypto, CryptoUtils;

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

procedure SaveStringToStream(Str: String; Stream: TStream);
begin
  SaveStringToStreamWord(Str,Stream);
end;

function LoadStringFromStream(Stream: TStream): String;
begin
  Result := LoadStringFromStreamWord(Stream);
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

procedure EncryptStream(InStream, OutStream: TStream; Password: String);
var
  Key: TKey256;
  Cipher: TCipher;
begin
  Key := SHA256String(Password);
  OutStream.Write(Key,SizeOf(Key));
  Cipher := CreateCipher(caRijndael,@Key,256);
  try
    Cipher.InitMode(cmCTR,nil);
    Cipher.EncryptStream(InStream,OutStream);
  finally
    Cipher.Free;
  end;
end;

procedure DecryptStream(InStream, OutStream: TStream; Password: String);
var
  Key1,Key2: TKey256;
  Cipher: TCipher;
begin
  Key1 := SHA256String(Password);
  InStream.Read(Key2,SizeOf(Key2));
  if CompareMem(@Key1,@Key2,SizeOf(TKey256)) = 0 then
    begin
      Cipher := CreateCipher(caRijndael,@Key1,256);
      try
        Cipher.InitMode(cmCTR,nil);
        Cipher.DecryptStream(InStream,OutStream);
      finally
        Cipher.Free;
      end;
    end
  else
    raise ECipher.Create('Invalid password');
end;

end.

