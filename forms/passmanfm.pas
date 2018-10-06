unit passmanfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Grids,
  ValEdit, ExtCtrls, ComCtrls, PasswordMan;

type

  { TPassManForm }

  TPassManForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    MenuItem2: TMenuItem;
    ExitItm: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveAsItm: TMenuItem;
    SaveDialog: TSaveDialog;
    SaveItm: TMenuItem;
    NewItm: TMenuItem;
    OpenItm: TMenuItem;
    StringGrid: TStringGrid;
    ToolBar1: TToolBar;
    AddBtn: TToolButton;
    DeleteBtn: TToolButton;
    procedure AddBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure ExitItmClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewItmClick(Sender: TObject);
    procedure OpenItmClick(Sender: TObject);
    procedure SaveAsItmClick(Sender: TObject);
    procedure SaveItmClick(Sender: TObject);
    procedure StringGridButtonClick(Sender: TObject; aCol, aRow: Integer);
  private
    procedure InitRow(I: Integer);
    function ValidateRow(I: Integer): Boolean;
    { private declarations }
  public
    PassMan: TPasswordManager;
    procedure GridToPassman;
    procedure PassmanToGrid;
  end;

var
  PassManForm: TPassManForm;

implementation

{$R *.lfm}

uses
  PassManParamsFM;

{ TPassManForm }

procedure TPassManForm.FormCreate(Sender: TObject);
begin
  PassMan := TPasswordManager.Create;
end;

procedure TPassManForm.ExitItmClick(Sender: TObject);
begin
  Close;
end;

procedure TPassManForm.InitRow(I: Integer);
begin
  StringGrid.Cells[0,I] := '1';
  StringGrid.Cells[3,I] := '0';
  StringGrid.Cells[4,I] := DateTimeToStr(Now);
end;

procedure TPassManForm.AddBtnClick(Sender: TObject);
begin
  StringGrid.RowCount := StringGrid.RowCount+1;
  InitRow(StringGrid.RowCount - 1);
end;

procedure TPassManForm.DeleteBtnClick(Sender: TObject);
begin
  if StringGrid.Selection.Top > 0 then
    StringGrid.DeleteRow(StringGrid.Selection.Top);
end;

procedure TPassManForm.FormDestroy(Sender: TObject);
begin
  PassMan.Free;
end;

procedure TPassManForm.NewItmClick(Sender: TObject);
begin
  Passman.Clear;
  PassmanToGrid;
  OpenDialog.Filename := '';
  SaveDialog.Filename := '';
end;

procedure TPassManForm.OpenItmClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    begin
      PassMan.LoadFromFile(OpenDialog.Filename);
      PassManToGrid;
      SaveDialog.Filename := OpenDialog.Filename;
    end;
end;

procedure TPassManForm.SaveAsItmClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    begin
      GridToPassMan;
      PassManToGrid;
      PassMan.SaveToFile(SaveDialog.Filename);
      OpenDialog.Filename := SaveDialog.Filename;
    end;
end;

procedure TPassManForm.SaveItmClick(Sender: TObject);
begin
  if SaveDialog.Filename = '' then
    begin
      if SaveDialog.Execute then
        begin
          GridToPassMan;
          PassManToGrid;
          PassMan.SaveToFile(SaveDialog.Filename);
        end;
    end
  else
    begin
      GridToPassMan;
      PassManToGrid;
      PassMan.SaveToFile(SaveDialog.Filename);
    end;
end;

procedure TPassManForm.StringGridButtonClick(Sender: TObject; aCol, aRow: Integer);
begin
  if aCol = 6 then
    begin
      ParamsForm.ValueListEditor.Clear;
      ParamsForm.ValueListEditor.Strings.Text := StringGrid.Cells[6,aRow];
      if ParamsForm.ShowModal = mrOK then
        StringGrid.Cells[6,aRow] := ParamsForm.ValueListEditor.Strings.Text;
    end;
end;

function TPassManForm.ValidateRow(I: Integer): Boolean;
begin
  Result := (StringGrid.Cells[0,I] > '') and
            (StringGrid.Cells[1,I] > '') and
            (StringGrid.Cells[2,I] > '') and
            (StringGrid.Cells[3,I] > '');
end;

procedure TPassManForm.GridToPassman;
var
  I: Integer;
  O: TPasswordManagerAccount;
begin
  PassMan.Clear;
  for I := 1 to StringGrid.RowCount - 1 do
    begin
      if ValidateRow(I) then
        begin
          O := TPasswordManagerAccount.Create(PassMan);
          O.Enabled := StringGrid.Cells[0,I] = '1';
          O.Username := StringGrid.Cells[1,I];
          O.Password := StringGrid.Cells[2,I];
          O.Admin := StringGrid.Cells[3,I] = '1';
          O.LastActive := StrToDateTime(StringGrid.Cells[4,I]);
          O.Description := StringGrid.Cells[5,I];
          O.Params.Text := StringGrid.Cells[6,I];
        end;
    end;
end;

procedure TPassManForm.PassmanToGrid;
var
  I: Integer;
  O: TPasswordManagerAccount;
begin
  StringGrid.RowCount := 1;
  StringGrid.RowCount := PassMan.Count + 1;
  for I := 0 to PassMan.Count - 1 do
    begin
      O := PassMan[I];
      if O.Enabled then
        StringGrid.Cells[0,I+1] := '1'
      else
        StringGrid.Cells[0,I+1] := '0';
      StringGrid.Cells[1,I+1] := O.Username;
      StringGrid.Cells[2,I+1] := O.Password;
      if O.Admin then
        StringGrid.Cells[3,I+1] := '1'
      else
        StringGrid.Cells[3,I+1] := '0';
      StringGrid.Cells[4,I+1] := DateTimeToStr(O.LastActive);
      StringGrid.Cells[5,I+1] := O.Description;
      StringGrid.Cells[6,I+1] := O.Params.Text;
    end;
end;

end.

