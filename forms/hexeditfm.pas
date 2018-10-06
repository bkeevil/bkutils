unit HexEditFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids;

type

  { THexEditForm }

  THexEditForm = class(TForm)
    CloseBtn: TButton;
    Grid: TStringGrid;
    procedure CloseBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    procedure Load(Stream: TStream);
    procedure Clear;
  end;

var
  HexEditForm: THexEditForm;

procedure HexEdit(Stream: TStream);

implementation

procedure HexEdit(Stream: TStream);
begin
  if not Assigned(HexEditForm) then
    HexEditForm := THexEditForm.Create(Application)
  else
    HexEditForm.Clear;
  HexEditForm.Load(Stream);
  HexEditForm.Show;
end;

{$R *.lfm}

{ THexEditForm }

procedure THexEditForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure THexEditForm.Load(Stream: TStream);
var
  RowCount: Integer;
  Row,Col: Integer;
  Data: Byte;
  ASCII: String;
begin
  Assert(Assigned(Stream));
  if not Assigned(Stream) then Exit;
  Stream.Position := 0;
  RowCount := Stream.Size div $10;
  Grid.RowCount := RowCount + 1;
  for Row := 1 to RowCount do
    begin
      ASCII := '';
      Grid.Cells[0,Row] := IntToHex(Stream.Position,8);
      for Col := 1 to $10 do
        begin
          Stream.Read(Data,1);
          ASCII := ASCII + Chr(Data);
          Grid.Cells[Col,Row] := IntToHex(Data,2);
        end;
      Grid.Cells[$11,Row] := ASCII;
    end;
end;

procedure THexEditForm.Clear;
begin
  Grid.Clear;
end;

end.
