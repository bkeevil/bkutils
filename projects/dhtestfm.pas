unit dhtestfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DiffieHellman;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { private declarations }
  public
    Client: TDiffieHellman128;
    Server: TDiffieHellman128;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  DateUtils;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Client := TDiffieHellman128.Create;
  Server := TDiffieHellman128.Create;
  Client.ReceiveResponse(Server.ProcessRequest(Client.GenerateRequest));
  if Client.KeysMatch(Server) then
    begin
      Memo1.Lines.Add(IntToStr(Client[0])+'.'+IntToStr(Client[1])+'.'+IntToStr(Client[2])+'.'+IntToStr(Client[3]));
    end
  else
    begin
      Memo1.Lines.Add('FAILED');
      Exit;
    end;
  Client.Destroy;
  Server.Destroy;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  S,E: TDateTime;
  X: Integer;
begin
  S := Time;
  for X := 1 to 1000 do
    Button1Click(nil);
  E := Time;
  Memo1.Lines.Add(IntToStr(MillisecondsBetween(S,E)));
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  DH: TDiffieHellman;
  I: Integer;
begin
  DH := TDiffieHellman.Create;
  try
    for I := 1 to 10 do
      begin
        Memo1.Lines.Add(IntToStr(GenerateRandom));
        Application.ProcessMessages;
      end;
  finally
    DH.Destroy;
  end;
end;

end.

