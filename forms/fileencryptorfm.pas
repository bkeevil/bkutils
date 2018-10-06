unit fileencryptorfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, Files, types;

const
  {$IFDEF Unix}
  DIRECTORY_SEPARATOR = '/';
  {$ELSE}
  DIRECTORY_SEPARATOR = '\';
  {$ENDIF}

type

  { TFileContainerForm }

  TFileContainerForm = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    ImageList: TImageList;
    DisabledImages: TImageList;
    ListView: TListView;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    CloseItm: TMenuItem;
    ImportItm: TMenuItem;
    ExportItm: TMenuItem;
    ExitItm: TMenuItem;
    DeleteItm: TMenuItem;
    MenuItem1: TMenuItem;
    HelpMenu: TMenuItem;
    AboutItm: TMenuItem;
    SecurityItm: TMenuItem;
    SecurityBtn: TToolButton;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    ToolButton5: TToolButton;
    PUImportItm: TMenuItem;
    PUDeleteItm: TMenuItem;
    PUExportItm: TMenuItem;
    NewItm: TMenuItem;
    OpenDialog: TOpenDialog;
    ListPopup: TPopupMenu;
    SaveDialog: TSaveDialog;
    NewBtn: TToolButton;
    ExportDialog: TSaveDialog;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    OpenItm: TMenuItem;
    CloseBtn: TToolButton;
    ToolButton1: TToolButton;
    ExitBtn: TToolButton;
    DeleteBtn: TToolButton;
    ToolButton3: TToolButton;
    ImportBtn: TToolButton;
    ExportBtn: TToolButton;
    ImportDialog: TOpenDialog;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    OpenBtn: TToolButton;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListViewEdited(Sender: TObject; Item: TListItem; var
      AValue: string);
    procedure OnNew(Sender: TObject);
    procedure OnClose(Sender: TObject);
    procedure OnDelete(Sender: TObject);
    procedure OnExit(Sender: TObject);
    procedure OnExport(Sender: TObject);
    procedure OnImport(Sender: TObject);
    procedure OnOpen(Sender: TObject);
    procedure OnSecurity(Sender: TObject);
    procedure StatusBarDrawPanel(AStatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure StatusBarResize(Sender: TObject);
  private
    FStatusBarPosition: Byte;
    procedure ExportFile(AFile: TFile; Filename: String);
    procedure HandleGetPassword(Sender: TObject; var Password: String);
    procedure ImportFile(Filename: String);
    procedure ReloadListView;
    procedure UpdateControls;
    function GetFile(Item: TListItem): TFile;
    procedure HandleProgress(Sender: TObject; Operation: TFileOperation; AName: String; APosition, ASize: Integer);
  public
    F: TFileContainer;
  end;

var
  FileContainerForm: TFileContainerForm;

implementation

uses
  SecurityFM, ProgressFM, PasswdFM;

{$R *.lfm}

{ TFileContainerForm }

procedure TFileContainerForm.StatusBarDrawPanel(AStatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
var
  R: TRect;
begin
  if Panel.Index = 1 then
    begin
      AStatusBar.Canvas.Brush.Color := clHotLight;
      R := Rect;
      R.Right := R.Left + Trunc((R.Right - R.Left) * FStatusBarPosition / 100);
      AStatusBar.Canvas.FillRect(R);
    end;
end;

procedure TFileContainerForm.StatusBarResize(Sender: TObject);
begin
  StatusBar.Panels[0].Width := StatusBar.ClientWidth - StatusBar.Panels[1].Width;
end;

procedure TFileContainerForm.UpdateControls;
begin
  NewItm.Enabled := not F.Active and (F.Operation = foNone);
  OpenItm.Enabled := not F.Active and (F.Operation = foNone);
  CloseItm.Enabled := F.Active and (F.Operation = foNone);
  ImportItm.Enabled := F.Active and (F.Files.Count < MAX_FILE_COUNT) and (F.Operation = foNone);
  ExportItm.Enabled := F.Active and (ListView.Selected <> nil) and (F.Operation = foNone);
  DeleteItm.Enabled := ExportItm.Enabled and (F.Operation = foNone);
  SecurityItm.Enabled := F.Active and (F.Operation = foNone);
  ExitItm.Enabled := F.Operation = foNone;

  PUImportItm.Enabled := ImportItm.Enabled;
  PUExportItm.Enabled := ExportItm.Enabled;
  PUDeleteItm.Enabled := DeleteItm.Enabled;

  NewBtn.Enabled := NewItm.Enabled;
  OpenBtn.Enabled := OpenItm.Enabled;
  CloseBtn.Enabled := CloseItm.Enabled;
  ImportBtn.Enabled := ImportItm.Enabled;
  ExportBtn.Enabled := ExportItm.Enabled;
  DeleteBtn.Enabled := DeleteItm.Enabled;
  SecurityBtn.Enabled := SecurityItm.Enabled;
  ExitBtn.Enabled := ExitItm.Enabled;
end;

function TFileContainerForm.GetFile(Item: TListItem): TFile;
begin
  Result := F.Files.Find(Item.Caption);
end;

procedure TFileContainerForm.HandleProgress(Sender: TObject; Operation: TFileOperation; AName: String; APosition, ASize: Integer);
var
  LPos: Byte;
  Percentage: Single;
  OpStr: String;
  StatusStr: String;
begin
  UpdateControls;
  if ASize = 0 then
    LPos := 0
  else
    LPos := Trunc(APosition / ASize * 100);
  if APosition = ASize then
    begin
      StatusBar.Panels[0].Text := '';
      LPos := 0;
    end
  else
    begin
      case Operation of
        foImport     : OpStr := 'Importing';
        foExport     : OpStr := 'Exporting';
        foDelete     : OpStr := 'Deleting';
        foDefrag     : OpStr := 'Defragging';
        foEncrypt    : OpStr := 'Encrypting';
        foDecrypt    : OpStr := 'Decrypting';
      end;
      if ASize > 0 then
        begin
          Percentage := APosition / ASize * 100;
          if AName > '' then
            StatusStr := Format('%s %%%3.0f of %s',[OpStr,Percentage,AName])
          else
            StatusStr := Format('%s %%%3f',[OpStr,Percentage]);
        end
      else
        if AName > '' then
          StatusStr := Format('%s %s',[OpStr,AName])
        else
          StatusStr := OpStr;
      if StatusBar.Panels[0].Text <> StatusStr then
        StatusBar.Panels[0].Text := StatusStr;
    end;
  //if PageMapForm.Visible then ShowPageMap(F);
  if LPos <> FStatusBarPosition then
    begin
      FStatusBarPosition := LPos;
      StatusBar.InvalidatePanel(1,[ppText]);
    end;
  Application.ProcessMessages;
end;

procedure TFileContainerForm.OnOpen(Sender: TObject);
begin
  F.Active := False;
  if OpenDialog.Execute then
    begin
      F.Filename := OpenDialog.FileName;
      Caption := OpenDialog.Filename;
      F.Active := True;
      ReloadListView;
    end;
end;

procedure TFileContainerForm.OnSecurity(Sender: TObject);
begin
  ShowSecurityDlg(F);
end;

function FileSizeStr(Size: Cardinal): String;
begin
  if Size > MaxInt then
    Result := Format('%5.0n GB',[Size / 1000000000])
  else if Size > 99999999 then
    Result := Format('%5.0n MB',[Size / 1000000])
  else if Size > 99999 then
    Result := Format('%5.0n KB',[Size / 1000])
  else
    Result := Format('%5.0n  B',[Size / 1])
end;

procedure TFileContainerForm.ReloadListView;
var
  I: Integer;
  FI: TFile;
  LI: TListItem;
begin
  ListView.Items.Clear;
  for I := 0 to F.Files.Count - 1 do
    begin
      FI := F.Files[I];
      LI := ListView.Items.Add;
      LI.ImageIndex := 8;
      LI.Caption := FI.Name;
      LI.Data := FI;
      LI.SubItems.Add(FileSizeStr(FI.Size));
    end;
end;

procedure TFileContainerForm.OnClose(Sender: TObject);
begin
  F.Active := False;
  ListView.Items.Clear;
  Caption := 'File Container';
end;

procedure TFileContainerForm.OnDelete(Sender: TObject);
var
  LI: TListItem;
  FI: TFile;
begin
  LI := ListView.Selected;
  FI := GetFile(LI);
  LI.Free;
  F.DeleteFile(FI.Name);
end;

procedure TFileContainerForm.FormCreate(Sender: TObject);
begin
  F := TFileContainer.Create;
  //F.AutoPack := True;         // TODO: Test this with delete file
  F.OnProgress := @HandleProgress;
  F.OnGetPassword := @HandleGetPassword;
end;

procedure TFileContainerForm.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  UpdateControls;
end;

procedure TFileContainerForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := F.Operation = foNone;
end;

procedure TFileContainerForm.FormDestroy(Sender: TObject);
begin
  F.Free;
end;

procedure TFileContainerForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  X: Integer;
begin
  if (F.Operation = foNone) and F.Active then
    for X := Low(Filenames) to High(Filenames) do
      ImportFile(Filenames[X]);
end;

procedure TFileContainerForm.HandleGetPassword(Sender: TObject; var Password: String);
begin
  //Password := PasswordBox(F.Filename,'Enter password');
  if not GetPassword(Password) then
    Password := '';
end;

procedure TFileContainerForm.ListViewCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := CompareText(Item1.Caption,Item2.Caption);
end;

procedure TFileContainerForm.ListViewEdited(Sender: TObject; Item: TListItem; var AValue: string);
var
  FI: TFile;
begin
  FI := TFile(Item.Data);
  if Assigned(FI) then
    FI.Name := AValue;
  //ReloadListView;
end;

procedure TFileContainerForm.OnNew(Sender: TObject);
begin
  F.Active := False;
  if SaveDialog.Execute then
    begin
      DeleteFile(SaveDialog.Filename);
      F.Filename := SaveDialog.Filename;
      F.Active := True;
      ListView.Items.Clear;
    end;
end;

procedure TFileContainerForm.OnExit(Sender: TObject);
begin
  if F.Active then
    OnClose(Sender);
  Close;
end;

procedure TFileContainerForm.OnExport(Sender: TObject);
var
  X: Integer;
  LI: TListItem;
  FI: TFile;
begin
  if ListView.SelCount = 1 then
    begin
      LI := ListView.Selected;
      FI := GetFile(LI);
      ExportDialog.Filename := FI.Name;
      if ExportDialog.Execute then
        ExportFile(FI,ExportDialog.Filename);
    end
  else
    begin
      if SelectDirectoryDialog.Execute then
        for X := 0 to ListView.Items.Count - 1 do
          begin
            LI := ListView.Items[X];
            if LI.Selected then
              begin
                FI := GetFile(LI);
                ExportFile(FI,SelectDirectoryDialog.FileName+DIRECTORY_SEPARATOR+FI.Name);
              end;
          end;
    end;
end;

procedure TFileContainerForm.ExportFile(AFile: TFile; Filename: String);
var
  S: TFileStream;
begin
  ProgressForm.Caption := 'Exporting ' + AFile.Name;
  ProgressForm.CaptionString := 'Exported %d of %d';
  S := TFileStream.Create(Filename,fmCreate);
  try
    F.ExportFile(S,AFile.Name);
  finally
    S.Free;
  end;
  F.Flush;
end;

procedure TFileContainerForm.ImportFile(Filename: String);
var
  S: TFileStream;
  FI: TFile;
begin
  S := TFileStream.Create(Filename,fmOpenRead);
  try
    Filename := ExtractFilename(Filename);
    ProgressForm.Caption := 'Importing ' + Filename;
    ProgressForm.CaptionString := 'Imported %d of %d';
    F.ImportFile(S,Filename);
    FI := F.Files.Find(Filename);
    Assert(FI<>nil);
    ReloadListView;
  finally
    S.Free;
  end;
  F.Flush;
end;

procedure TFileContainerForm.OnImport(Sender: TObject);
var
  X: Integer;
begin
  if ImportDialog.Execute then
    for X := 0 to ImportDialog.Files.Count - 1 do
      ImportFile(ImportDialog.Files[X]);
end;

end.

