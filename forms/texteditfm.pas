unit texteditfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, ActnList, StdActns;

type

  { TTextEditForm }

  TTextEditForm = class(TForm)
    ActionList1: TActionList;
    CloseBtn: TButton;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditPaste1: TEditPaste;
    EditSelectAll2: TEditSelectAll;
    EditUndo1: TEditUndo;
    ImageList1: TImageList;
    Memo: TMemo;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure CloseBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  TextEditForm: TTextEditForm;

procedure TextEdit(Str: String);

implementation

procedure TextEdit(Str: String);
begin
  if not Assigned(TextEditForm) then
    TextEditForm := TTextEditForm.Create(Application)
  else
    TextEditForm.Memo.Clear;
  TextEditForm.Memo.Lines.Text := Str;
  TextEditForm.Show;
end;

{$R *.lfm}

{ TTextEditForm }

procedure TTextEditForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

end.

