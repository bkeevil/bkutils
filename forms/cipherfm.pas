unit cipherfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Crypto, CryptoUtils, DiffieHellman;

type

  { TCipherForm }

  TCipherForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    DHC,DHS: TDiffieHellman128;
    CKey: TKey;
    SKey: TKey;
    IV: TBlock;
    CC: TCustomCipher;
    SC: TCustomCipher;
    procedure DumpBytes(B: Pointer; S: Word);
  public
    { public declarations }
  end;

var
  CipherForm: TCipherForm;

implementation

{$R *.lfm}

{ TCipherForm }

procedure TCipherForm.DumpBytes(B: Pointer; S: Word);
var
  X: Integer;
  P: PByte;
  R: String;
begin
  for X := 0 to S - 1 do
    begin
      P := PByte(PtrInt(B)+X);
      R := R + IntToStr(P^) + ' ';
    end;
  Memo1.Lines.Add(R);
end;

procedure TCipherForm.Button1Click(Sender: TObject);
var
  B: PChar;
begin
  GetMem(B,200);
  //CC.ReadKeyStream(B,200);
  DumpBytes(B,200);
  Memo1.Lines.Add('');
  //SC.ReadKeyStream(B,200);
  DumpBytes(B,200);
  FreeMem(B,200);
end;

procedure TCipherForm.Button2Click(Sender: TObject);
var
  X: Byte;
  S: Pointer;
begin
  GetMem(S,200);
  for X := 0 to 200 do
    PByte(PtrInt(S)+X)^ := X;
  CC.Encrypt(S,200);
  SC.Decrypt(S,200);
  DumpBytes(S,200);
  FreeMem(S);
end;

procedure TCipherForm.Button3Click(Sender: TObject);
var
  X: Integer;
  Y: Integer;
begin
  for X := 1 to 1024 * 256 do
    begin
      Y := X;
      CC.Encrypt(@Y,SizeOf(Integer));
      SC.Decrypt(@Y,SizeOf(Integer));
      if X <> Y then
        Memo1.Lines.Add('fail');
    end;
  Memo1.Lines.Add('Done');
end;

procedure TCipherForm.FormCreate(Sender: TObject);
var
  RQ: TDiffieHellman128Request;
  RS: TDiffieHellman128Response;
  X: Integer;
begin
  DHC := TDiffieHellman128.Create;
  RQ := DHC.GenerateRequest;
  DHS := TDiffieHellman128.Create;
  RS := DHS.ProcessRequest(RQ);
  DHC.ReceiveResponse(RS);
  for X := 0 to 15 do
    begin
      CKey[X] := DHC.Key[X];
      SKey[X] := DHS.Key[X];
    end;
  Randomize;
  IV := RandomBlock;
  CC := TIDEAOFBCipher.Create(CKey,IV);
  SC := TIDEAOFBCipher.Create(SKey,IV);
end;

procedure TCipherForm.FormDestroy(Sender: TObject);
begin
  DHC.Destroy;
  DHS.Destroy;
  CC.Destroy;
  SC.Destroy;
end;

end.

