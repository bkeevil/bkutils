unit ProgressFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls;

type

  { TProgressForm }

  TProgressForm = class(TForm)
    LBCaption: TLabel;
    ProgressBar: TProgressBar;
  private
    { private declarations }
  public
    CaptionString: String;
    procedure Progress(APosition, ASize: Cardinal);
  end;

var
  ProgressForm: TProgressForm;

implementation

{$R *.lfm}

{ TProgressForm }

procedure TProgressForm.Progress(APosition, ASize: Cardinal);
begin
  ProgressBar.Max := ASize;
  ProgressBar.Position := APosition;
  if APosition = 0 then
    Show
  else
    if APosition = ASize then
      Hide;
  LBCaption.Caption := Format(CaptionString,[APosition,ASize]);
  Application.ProcessMessages;
end;

end.

