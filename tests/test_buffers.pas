unit test_buffers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, buffers;

type

  { TTestBuffers }

  TTestBuffers = class(TTestCase)
  private
    function PeekString(Len: Integer): String;
    procedure PrependString(S: String);
  protected
    B: TBuffer;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure WriteString(S: String);
    function ReadString(Len: Integer): String;
  published
    procedure Test1;
    procedure Test2;
  end;

implementation

procedure TTestBuffers.Test1;
begin
  WriteString('');
  WriteString('A');
  WriteString('BC');
  WriteString('DEF');
  WriteString('GHIJ');
  WriteString('KLM');
  WriteString('NO');
  WriteString('P');
  WriteString('');
  AssertEquals(16,B.Size);
  AssertEquals('A',PeekString(1));
  AssertEquals('ABCDEF',PeekString(6));
  AssertEquals('ABCDEFG',PeekString(7));
  AssertEquals(16,B.Size);
  AssertEquals('AB',ReadString(2));
  AssertEquals(14,B.Size);
  AssertEquals('CDEFGHIJ',PeekString(8));
  AssertEquals('C',ReadString(1));
  AssertEquals('DEF',PeekString(3));
  AssertEquals('DEFGHI',ReadString(6));
  AssertEquals('JK',PeekString(2));
  AssertEquals('JK',ReadString(2));
  AssertEquals('LMNOP',ReadString(5));
  //
  AssertEquals(0,B.Size);
end;

procedure TTestBuffers.Test2;
begin
  WriteString('GHIJ');
  WriteString('KLM');
  WriteString('NO');
  WriteString('P');
  PrependString('');
  PrependString('DEF');
  PrependString('BC');
  PrependString('A');
  AssertEquals(16,B.Size);
  AssertEquals('ABCDEFGHIJKLMNOP',PeekString(16));
  B.Clear;
  AssertEquals(0,B.Size);
  WriteString('GHIJ');
  WriteString('KLM');
  WriteString('NO');
  WriteString('P');
  AssertEquals(10,B.Size);
  AssertEquals('GHI',ReadString(3));
  AssertEquals(7,B.Size);
  PrependString('DEF');
  AssertEquals(10,B.Size);
  AssertEquals('DEFJKLMNOP',PeekString(10));
  AssertEquals('D',ReadString(1));
  AssertEquals(9,B.Size);
  PrependString('ABCD');
  AssertEquals('ABCDEFJKLMNOP',PeekString(13));
  AssertEquals(13,B.Size);
  AssertEquals('ABCDEFJ',ReadString(7));
  PrependString('GHIJ');
  PrependString('DEF');
  PrependString('ABC');
  AssertEquals(16,B.Size);
  AssertEquals('ABCDEFGHIJKLMNOP',ReadString(16));
  AssertEquals(0,B.Size);
end;

procedure TTestBuffers.SetUp;
begin
  B := TBuffer.Create;
end;

procedure TTestBuffers.TearDown;
begin
  B.Destroy;
end;

procedure TTestBuffers.WriteString(S: String);
begin
  B.Write(PChar(S),Length(S));
end;

procedure TTestBuffers.PrependString(S: String);
begin
  B.Prepend(PChar(S),Length(S));
end;

function TTestBuffers.ReadString(Len: Integer): String;
var
  P: PChar;
begin
  P := GetMem(Len+1);
  FillChar(P^,Len+1,0);
  B.Read(P,Len);
  Result := StrPas(P);
end;

function TTestBuffers.PeekString(Len: Integer): String;
var
  P: PChar;
begin
  P := GetMem(Len+1);
  FillChar(P^,Len+1,0);
  B.Peek(P,Len);
  Result := StrPas(P);
end;

initialization

  RegisterTest(TTestBuffers);
end.

