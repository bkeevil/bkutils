unit test_base32;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestBase32 }

  TTestBase32= class(TTestCase)
  published
    //procedure EncodeDecodeQuantum;
    //procedure QuantumToStrToQuantum;
    procedure RFCTestVectors;
  end;

implementation

uses
  Base32;

{ TTestBase32 }

{procedure TTestBase32.EncodeDecodeQuantum;
var
  E: TEncodedQuantum;
  D: TDecodedQuantum;
begin
  D[0] := ord('A'); D[1] := ord('B'); D[2] := ord('C'); D[3] := ord('D'); D[4] := ord('E');
  E := EncodeQuantum(D);
  D := DecodeQuantum(E);
  AssertEquals(ord('A'),D[0]);
  AssertEquals(ord('B'),D[1]);
  AssertEquals(ord('C'),D[2]);
  AssertEquals(ord('D'),D[3]);
  AssertEquals(ord('E'),D[4]);
end;

procedure TTestBase32.QuantumToStrToQuantum;
var
  E: TEncodedQuantum;
  D: TDecodedQuantum;
  S: String;
begin
  D[0] := 2; D[1] := 255; D[2] := 67; D[3] := 31; D[4] := 201;
  E := EncodeQuantum(D);
  S := QuantumToStr(E);
  E := StrToQuantum(S);
  D := DecodeQuantum(E);
  AssertEquals(2,D[0]);
  AssertEquals(255,D[1]);
  AssertEquals(67,D[2]);
  AssertEquals(31,D[3]);
  AssertEquals(201,D[4]);
end;}

procedure TTestBase32.RFCTestVectors;
var  // Test Data from RFC4648
  P: array[0..63] of Char;
  S: String;
begin
  P := '';
  S := EncodeBase32(Pchar(P),0);
  AssertEquals('',S);
  AssertEquals('',DecodeBase32Str(S));
  P := 'f';
  S := EncodeBase32(PChar(P),1);
  AssertEquals('MY======',S);
  AssertEquals('f',DecodeBase32Str(S));
  P := 'fo';
  S := EncodeBase32(PChar(P),2);
  AssertEquals('MZXQ====',S);
  AssertEquals('fo',DecodeBase32Str(S));
  P := 'foo';
  S := EncodeBase32(PChar(P),3);
  AssertEquals('MZXW6===',S);
  AssertEquals('foo',DecodeBase32Str(S));
  P := 'foob';
  S := EncodeBase32(PChar(P),4);
  AssertEquals('MZXW6YQ=',S);
  AssertEquals('foob',DecodeBase32Str(S));
  P := 'fooba';
  S := EncodeBase32(PChar(P),5);
  AssertEquals('MZXW6YTB',S);
  AssertEquals('fooba',DecodeBase32Str(S));
  P := 'foobar';
  S := EncodeBase32(PChar(P),6);
  AssertEquals('MZXW6YTBOI======',S);
  AssertEquals('foobar',DecodeBase32Str(S));
end;

initialization
  RegisterTest(TTestBase32);
end.

