unit Base32;
{ base32.pas - A unit to do Base32 encoding/decoding as per RFC 4648
  Copyright (C) 2013  H. Bond Keevil

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

  DOCUMENTATION IS HERE:
  http://prognosti.com/index.php?option=com_content&view=article&id=16

  For assistance or support: bkeevil@prognosti.com }

{$mode objfpc}{$H+}

interface

function EncodeBase32(Source: Pointer; Size: Integer): String;
procedure DecodeBase32(Source: String; var Dest: Pointer; var Size: Integer);
function EncodeBase32Str(Source: String): String;
function DecodeBase32Str(Source: String): String;

implementation

uses
  SysUtils;

const
  MAP: array[0..32] of Char = ('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q',
                               'R','S','T','U','V','W','X','Y','Z','2','3','4','5','6','7','=');

type
  TEncodedQuantum = array[0..7] of Byte;
  TDecodedQuantum = array[0..4] of Byte;
  TDecodedQuantumArray = array of TDecodedQuantum;

function EncodeQuantum(Q: TDecodedQuantum): TEncodedQuantum;
begin
  Result[0] := (Q[0] and $F8) shr 3;
  Result[1] := ((Q[0] and $07) shl 2) or ((Q[1] and $FC) shr 6);
  Result[2] := (Q[1] and $3E) shr 1;
  Result[3] := ((Q[1] and $01) shl 4) or ((Q[2] and $F0) shr 4);
  Result[4] := ((Q[2] and $0F) shl 1) or ((Q[3] and $80) shr 7);
  Result[5] := ((Q[3] and $7C) shr 2);
  Result[6] := ((Q[3] and $03) shl 3) or ((Q[4] and $E0) shr 5);
  Result[7] := Q[4] and $1F
end;

function DecodeQuantum (Q: TEncodedQuantum): TDecodedQuantum;
begin
  Result[0] := (Q[0] shl 3) or ((Q[1] and $1C) shr 2);
  Result[1] := ((Q[1] and $03) shl 6) or (Q[2] shl 1) or ((Q[3] and $10) shr 4);
  Result[2] := ((Q[3] and $0F) shl 4) or ((Q[4] and $1E) shr 1);
  Result[3] := ((Q[4] and $01) shl 7) or (Q[5] shl 2) or ((Q[6] and $18) shr 3);
  Result[4] := ((Q[6] and $07) shl 5) or (Q[7] and $1F);
end;

function QuantumToStr(Q: TEncodedQuantum): String;
begin
  Result := MAP[Q[0]]+MAP[Q[1]]+MAP[Q[2]]+MAP[Q[3]]+MAP[Q[4]]+MAP[Q[5]]+MAP[Q[6]]+MAP[Q[7]];
end;

function CharToBase32(C: Char): Byte;
var
  O: Byte;
begin
  O := Ord(C);
  if (O > 64) and (O < 91) then
    Result := O-65
  else if (O > 49) and (O < 56) then
    Result := O-50+26
  else if (O = 61) then
    Result := 32
  else
    raise EConvertError.Create('Invalid Base32 character');
end;

function StrToQuantum(S: String): TEncodedQuantum;
var
  I: Integer;
begin
  for I := 0 to 7 do
    Result[I] := CharToBase32(S[I+1]);
end;

function GetDecodedQuantum(Data: Pointer; Size, Index: Integer): TDecodedQuantum;
var
  I, First, Last: Integer;
begin
  FillChar(Result,SizeOf(Result),0);
  // Find the range of bytes to copy in Data
  First := Index * 5;
  Last := (Index + 1) * 5;
  if Last > Size then
    Last := Size;
  // Copy the bytes
  for I := First to Last - 1 do
     Result[I - First] := PByte(PtrUInt(Data)+I)^;
end;

procedure SetDecodedQuantum(Data: Pointer; Size, Index: Integer; Value: TDecodedQuantum);
var
  I, First, Last: Integer;
begin
  // Find the range of bytes to copy in Data
  First := Index * 5;
  Last := (Index + 1) * 5;
  if Last > Size then
    Last := Size;
  for I := First to Last - 1 do
    PByte(PtrUInt(Data)+I)^ := Value[I - First];
end;

function EncodeBase32(Source: Pointer; Size: Integer): String;
var
  I, C: Integer;
  D: TDecodedQuantum;
  E: TEncodedQuantum;
begin
  Result := '';
  //if Size = 0 then Exit;
  C := (Size div 5);
  if Size mod 5 = 0 then
    dec(C);
  // Encode full quantum blocks
  for I := 0 to C do
    begin
      D := GetDecodedQuantum(Source,Size,I);
      E := EncodeQuantum(D);
      Result := Result + QuantumToStr(E);
    end;

  // Add padding indicator
  case Size mod 5 of
    1: Result := copy(Result,1,Length(Result)-6) + '======';
    2: Result := copy(Result,1,Length(Result)-4) + '====';
    3: Result := copy(Result,1,Length(Result)-3) + '===';
    4: Result := copy(Result,1,Length(Result)-1) + '=';
  end;
end;

function PaddingCount(Str: String): Byte;
var
  I: Integer;
begin
  I := Length(Str);
  Result := 0;
  while (I > 0) and (Str[I] = '=') do
    begin
      inc(Result);
      dec(I);
    end;
end;

procedure DecodeBase32(Source: String; var Dest: Pointer; var Size: Integer);
var
  PC: Byte;             // The number of '=' characters at the end of Source
  I: Integer;           // Index of Quantum in Dest (as if Dest were a zero based array of TDecodedQuantum)
  Pos: Integer;         // Position of current character in Source string
  QStr: String;         // Accumulates a string representation of a quantum
  E: TEncodedQuantum;
  D: TDecodedQuantum;
begin
  if Source = '' then
    begin
      Dest := nil;
      Size := 0;
      Exit;
    end;
  PC := PaddingCount(Source);
  Size := ((Length(Source) - PC) * 5 div 8);
  GetMem(Dest,Size);

  I := 0;
  QStr := '';
  for Pos := 1 to Length(Source) do
    begin
      QStr := QStr + Source[Pos];
      if Length(QStr) = 8 then
        begin
          E := StrToQuantum(QStr);
          D := DecodeQuantum(E);
          SetDecodedQuantum(Dest,Size,I,D);
          inc(I);
          QStr := '';
        end;
    end;
  Assert(QStr = '');
end;

function EncodeBase32Str(Source: String): String;
begin
  Result := EncodeBase32(PChar(Source),Length(Source));
end;

function DecodeBase32Str(Source: String): String;
var
  Buffer: PChar;
  Size: Integer;
begin
  DecodeBase32(Source,Buffer,Size);
  Result := Copy(StrPas(Buffer),1,Size);
  FreeMem(Buffer,Size);
end;

end.

