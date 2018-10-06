unit FileContainerFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, Files;

type

  { TFileContainerForm }

  TFileContainerForm = class(TForm)
    ImageList: TImageList;
    DisabledImages: TImageList;
    ListView: TListView;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    CloseItm: TMenuItem;
    ImportItm: TMenuItem;
    ExportItm: TMenuItem;
    ExitItm: TMenuItem;
    ListItm: TMenuItem;
    IconsItm: TMenuItem;
    DeleteItm: TMenuItem;
    HeaderBtn: TToolButton;
    FATBtn: TToolButton;
    CreateTestFileItm: TMenuItem;
    AddSampleDataItm: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    DeleteSampleDataItm: TMenuItem;
    InsertSampleDataItm: TMenuItem;
    DefragItm: TMenuItem;
    EncryptItm: TMenuItem;
    MenuItem5: TMenuItem;
    DecryptItm: TMenuItem;
    PackItm: TMenuItem;
    ValidateSampleDataItm: TMenuItem;
    ValidateFreePagesItm: TMenuItem;
    ValidatePageMapItm: TMenuItem;
    ViewLogItm: TMenuItem;
    ValidateTestFileItm: TMenuItem;
    SampleDataTestItm: TMenuItem;
    FlushItm: TMenuItem;
    OperationsMenu: TMenuItem;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ViewFATItm: TMenuItem;
    ViewMapItm: TMenuItem;
    PUHexEditItm: TMenuItem;
    MenuItem1: TMenuItem;
    HexEditItm: TMenuItem;
    TextEditItm: TMenuItem;
    PUImportItm: TMenuItem;
    PUDeleteItm: TMenuItem;
    PUExportItm: TMenuItem;
    PUTextEditItm: TMenuItem;
    NewItm: TMenuItem;
    OpenDialog: TOpenDialog;
    ListPopup: TPopupMenu;
    SaveDialog: TSaveDialog;
    Separator3: TMenuItem;
    NewBtn: TToolButton;
    HexEditBtn: TToolButton;
    TextEditBtn: TToolButton;
    MapBtn: TToolButton;
    ViewHeaderItm: TMenuItem;
    ExportDialog: TSaveDialog;
    SmallIconsItm: TMenuItem;
    ReportItm: TMenuItem;
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
    ViewMenu: TMenuItem;
    ImportDialog: TOpenDialog;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    OpenBtn: TToolButton;
    procedure DecryptItmClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure OnDefrag(Sender: TObject);
    procedure OnDeleteSampleData(Sender: TObject);
    procedure OnEncryptionChecked(Sender: TObject);
    procedure OnInsertSampleData(Sender: TObject);
    procedure OnPack(Sender: TObject);
    procedure OnSampleDataTest(Sender: TObject);
    procedure OnFlush(Sender: TObject);
    procedure OnAddSampleData(Sender: TObject);
    procedure OnCreateTestFile(Sender: TObject);
    procedure OnHexEdit(Sender: TObject);
    procedure OnMap(Sender: TObject);
    procedure OnNew(Sender: TObject);
    procedure OnClose(Sender: TObject);
    procedure OnDelete(Sender: TObject);
    procedure OnExit(Sender: TObject);
    procedure OnExport(Sender: TObject);
    procedure OnImport(Sender: TObject);
    procedure OnOpen(Sender: TObject);
    procedure OnTextEdit(Sender: TObject);
    procedure OnValidateFreePageList(Sender: TObject);
    procedure OnValidatePageMap(Sender: TObject);
    procedure OnValidateSampleData(Sender: TObject);
    procedure OnViewChange(Sender: TObject);
    procedure OnViewFAT(Sender: TObject);
    procedure OnViewHeader(Sender: TObject);
    procedure StatusBarDrawPanel(AStatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure StatusBarResize(Sender: TObject);
    procedure ValidateTestFileItmClick(Sender: TObject);
    procedure ViewLogItmClick(Sender: TObject);
  private
    FStatusBarPosition: Byte;
    function GetKindName(Kind: TFileKind): String;
    procedure HandleGetPassword(Sender: TObject; var Password: String);
    procedure ReloadListView;
    procedure UpdateControls;
    procedure UpdatePageMap;
    function GetFile(Item: TListItem): TFile;
    procedure HandleProgress(Sender: TObject; Operation: TFileOperation; AName: String; APosition, ASize: Integer);
    procedure ValidateSampleFile(Number: Integer);
  public
    F: TFileContainer;
  end;

var
  FileContainerForm: TFileContainerForm;

implementation

uses
  ContainerFM, HexEditFM, TextEditFM, PageMapFM, FATFM, ProgressFM, DebugLogFM, PasswdFM;

{$R *.lfm}

{ TFileContainerForm }

procedure TFileContainerForm.OnViewChange(Sender: TObject);
begin
  if Sender = ListItm then
    ListView.ViewStyle := vsList;
  if Sender = ReportItm then
    ListView.ViewStyle := vsReport;
  if Sender = IconsItm then
    ListView.ViewStyle := vsIcon;
  if Sender = SmallIconsItm then
    ListView.ViewStyle := vsSmallIcon;
end;

procedure TFileContainerForm.OnViewFAT(Sender: TObject);
begin
  ShowFATInspector(F);
end;

procedure TFileContainerForm.OnViewHeader(Sender: TObject);
begin
  ShowContainerProperties(F);
end;

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

procedure TFileContainerForm.ValidateSampleFile(Number: Integer);
var
  I,J: Integer;
  C,D: Char;
  FN: String;
  H: TStreamFileHandle;
begin
  FN := 'test data '+IntToStr(Number);
  if not F.FileExists(FN) then Exit;
  H := F.OpenFile(FN,fkStream) as TStreamFileHandle;
  try
    for J := 1 to 50 do
      for C := 'A' to 'Z' do
        for I := 1 to 4096 do
          begin
            H.Read(D,1);
            if D <> C then raise EFileError.Create('Validation Failed');
          end;
    StatusBar.Panels[0].Text := 'Validation of '+H._File.Name+' passed';
    Application.ProcessMessages;
  finally
    H.Destroy;
  end;
end;

procedure TFileContainerForm.ValidateTestFileItmClick(Sender: TObject);
var
  I,J: Integer;
  C,D: Char;
  H: TStreamFileHandle;
begin
  if not Assigned(ListView.Selected) then Exit;
  H := F.OpenFile(ListView.Selected.Caption) as TStreamFileHandle;
  try
    for J := 1 to 50 do
      for C := 'A' to 'Z' do
        for I := 1 to 4096 do
          begin
            H.Read(D,1);
            if D <> C then raise EFileError.Create('Validation Failed');
          end;
    StatusBar.Panels[0].Text := 'Validation of '+H._File.Name+' passed';
  finally
    H.Destroy;
  end;
end;

procedure TFileContainerForm.ViewLogItmClick(Sender: TObject);
begin
  DebugLogForm.Show;
end;

procedure TFileContainerForm.UpdateControls;
begin
  NewItm.Enabled := not F.Active;
  OpenItm.Enabled := not F.Active;
  CloseItm.Enabled := F.Active;
  ImportItm.Enabled := F.Active and (F.Files.Count < MAX_FILE_COUNT);
  ExportItm.Enabled := F.Active and (ListView.Selected <> nil);
  DeleteItm.Enabled := ExportItm.Enabled;
  HexEditItm.Enabled := ExportItm.Enabled;
  TextEditItm.Enabled := ExportItm.Enabled;
  ViewMapItm.Enabled := F.Active;
  ViewHeaderItm.Enabled := F.Active;
  ViewFATItm.Enabled := F.Active;
  FlushItm.Enabled := F.Active;

  PUImportItm.Enabled := ImportItm.Enabled;
  PUExportItm.Enabled := ExportItm.Enabled;
  PUDeleteItm.Enabled := DeleteItm.Enabled;
  PUHexEditItm.Enabled := HexEditItm.Enabled;
  PUTextEditItm.Enabled := TextEditItm.Enabled;

  NewBtn.Enabled := NewItm.Enabled;
  OpenBtn.Enabled := OpenItm.Enabled;
  CloseBtn.Enabled := CloseItm.Enabled;
  ImportBtn.Enabled := ImportItm.Enabled;
  ExportBtn.Enabled := ExportItm.Enabled;
  DeleteBtn.Enabled := DeleteItm.Enabled;
  HexEditBtn.Enabled := PUHexEditItm.Enabled;
  TextEditBtn.Enabled := PUTextEditItm.Enabled;
  MapBtn.Enabled := ViewMapItm.Enabled;
  HeaderBtn.Enabled := ViewHeaderItm.Enabled;
  FATBtn.Enabled := ViewFATItm.Enabled;
end;

procedure TFileContainerForm.UpdatePageMap;
begin
  ShowPageMap(F);
end;

function TFileContainerForm.GetFile(Item: TListItem): TFile;
begin
  Result := F.Files.Find(Item.Caption);
end;

procedure TFileContainerForm.HandleProgress(Sender: TObject; Operation: TFileOperation; AName: String; APosition, ASize: Integer);
var
  LPos: Byte;
begin
  if ASize = 0 then
    LPos := 0
  else
    LPos := Trunc(APosition / ASize * 100);
  if LPos = 100 then
    begin
      StatusBar.Panels[0].Text := '';
      LPos := 0;
    end
  else
    case Operation of
      foImport: StatusBar.Panels[0].Text := 'Importing '+AName;
      foExport: StatusBar.Panels[0].Text := 'Exporting '+AName;
      foDelete: StatusBar.Panels[0].Text := 'Deleting '+AName;
      foDefrag: StatusBar.Panels[0].Text := 'Defragging '+AName;
      foEncrypt: StatusBar.Panels[0].Text := 'Encrypting '+AName;
      foDecrypt: StatusBar.Panels[0].Text := 'Decrypting '+AName;
    end;
{  case Operation of
    foImport: StatusBar.Panels[0].Text := Format('Importing %%%3f',[APosition/ASize*100]);
    foExport: StatusBar.Panels[0].Text := Format('Exporting %%%3f',[APosition/ASize*100]);
    foDelete: StatusBar.Panels[0].Text := Format('Deleting %%%3f',[APosition/ASize*100]);
    foDefrag: StatusBar.Panels[0].Text := Format('Defragging %%%3f',[APosition/ASize*100]);
    foEncrypt: StatusBar.Panels[0].Text := Format('Encrypting %%%3f',[APosition/ASize*100]);
    foDecrypt: StatusBar.Panels[0].Text := Format('Decrypting %%%3f',[APosition/ASize*100]);
  end;}
  //if PageMapForm.Visible then ShowPageMap(F);
  if LPos <> FStatusBarPosition then
    begin
      FStatusBarPosition := LPos;
      StatusBar.InvalidatePanel(1,[ppText]);
    end;
  Application.ProcessMessages;
end;

function TFileContainerForm.GetKindName(Kind: TFileKind): String;
begin
  case Kind of
    fkStream: Result := 'Stream';
    fkPage: Result := 'Page';
    fkBTree: Result := 'BTree';
  end;
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
    end
  else
    UpdateControls;
 // UpdatePageMap;
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
      LI.Caption := FI.Name;
      LI.SubItems.Add(IntToStr(FI.Size));
      LI.SubItems.Add(GetKindName(FI.Kind));
      LI.SubItems.Add(IntToStr(FI.PageCount));
      LI.SubItems.Add(IntToStr(FI.RecordCount));
      LI.SubItems.Add(IntToStr(FI.RootNode));
      LI.SubItems.Add(IntToStr(FI.ID));
    end;
  UpdateControls;
end;

procedure TFileContainerForm.OnTextEdit(Sender: TObject);
var
  Selected: TListItem;
  Stream: TStringStream;
begin
  Selected := ListView.Selected;
  Stream := TStringStream.Create('');
  try
    F.ExportFile(Stream,Selected.Caption);
    TextEdit(Stream.DataString);
  finally
    Stream.Free;
  end;
end;

procedure TFileContainerForm.OnValidateFreePageList(Sender: TObject);
begin
  if F.FreePages.CheckOrder then
    StatusBar.Panels[0].Text := 'Free page order check passed'
  else
    StatusBar.Panels[0].Text := 'Free page list is out of order';
end;

procedure TFileContainerForm.OnValidatePageMap(Sender: TObject);
begin
  if F.PageMap.Validate then
    StatusBar.Panels[0].Text := 'Missing page check passed'
  else
    StatusBar.Panels[0].Text := 'Validation failed.  Check log for errors';
end;

procedure TFileContainerForm.OnValidateSampleData(Sender: TObject);
var
  X: Integer;
begin
  for X := 1 to 16 do
    ValidateSampleFile(X);
end;

procedure TFileContainerForm.OnClose(Sender: TObject);
begin
  F.Active := False;
  ListView.Items.Clear;
  UpdateControls;
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
  UpdatePageMap;
end;

procedure TFileContainerForm.FormCreate(Sender: TObject);
begin
  F := TFileContainer.Create;
  //F.AutoPack := True;
  F.OnProgress := @HandleProgress;
  if not Assigned(DebugLogForm) then DebugLogForm := TDebugLogForm.Create(Self);
  F.OnGetPassword := @HandleGetPassword;
end;

procedure TFileContainerForm.DecryptItmClick(Sender: TObject);
begin
  F.Decrypt;
end;

procedure TFileContainerForm.FormDestroy(Sender: TObject);
begin
  F.Free;
end;

procedure TFileContainerForm.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

procedure TFileContainerForm.HandleGetPassword(Sender: TObject; var Password: String);
begin
  if not GetPassword(Password) then
    Password := '';
end;

procedure TFileContainerForm.ListViewCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := CompareText(Item1.Caption,Item2.Caption);
end;

procedure TFileContainerForm.ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  UpdateControls;
end;

procedure TFileContainerForm.OnDefrag(Sender: TObject);
begin
  F.Defrag;
  ReloadListView;
  ShowPageMap(F);
end;

procedure TFileContainerForm.OnDeleteSampleData(Sender: TObject);
var
  X: Integer;
  FN: String;
begin
  for X := 1 to 16 do
    begin
      FN := 'test data '+IntToStr(X);
      StatusBar.Panels[0].Text := 'Deleting '+FN;
      Application.ProcessMessages;
      F.DeleteFile(FN);
      ReloadListView;
      //OnValidateSampleData(nil);
      //OnValidatePageMap(nil);
      //OnValidateFreePageList(nil);
    end;
  ReloadListView;
end;

procedure TFileContainerForm.OnEncryptionChecked(Sender: TObject);
begin
  //F.Encrypt(F.;
end;

procedure TFileContainerForm.OnInsertSampleData(Sender: TObject);
var
  Page: PBufferedPage;
  X,Y: Integer;
  C: Char;
  H: array[1..16] of TPageFileHandle;
begin
  for X := 1 to 16 do
    H[X] := F.CreateFile('test data '+IntToStr(X),fkPage) as TPageFileHandle;

  for X := 1 to 50 do
    for C := 'Z' downto 'A' do
      for Y := 1 to 16 do
        begin
          Page := H[Y].Insert(0);
          FillChar(Page^.Data^,PAGE_SIZE,C);
          Page^.Modified := True;
          H[Y].Release(Page);
          H[Y].Flush;
        end;
  for X := 1 to 16 do
    H[X].Destroy;

  ReloadListView;
end;

procedure TFileContainerForm.OnPack(Sender: TObject);
begin
  F.Pack;
end;

procedure TFileContainerForm.OnSampleDataTest(Sender: TObject);
var
  StartTime,EndTime,Duration: TDateTime;
begin
  StartTime := Now;
  OnCreateTestFile(nil);
  OnAddSampleData(nil);
  F.Validate;
  OnValidateSampleData(nil);
  OnDeleteSampleData(nil);
  F.Validate;
  //
  OnCreateTestFile(nil);
  OnInsertSampleData(nil);
  OnDefrag(nil);
  F.Validate;
  OnValidateSampleData(nil);
  OnDeleteSampleData(nil);
  F.Validate;
  EndTime := Now;
  Duration := EndTime - StartTime;
  StatusBar.Panels[0].Text := 'Duration='+FormatDateTime('n:ss.zzz',Duration);
end;

procedure TFileContainerForm.OnFlush(Sender: TObject);
begin
  F.Flush;
end;

procedure TFileContainerForm.OnAddSampleData(Sender: TObject);
var
  S: TMemoryStream;
  X,Y: Integer;
  C: Char;
  H: array[1..16] of TStreamFileHandle;
begin
  S := TMemoryStream.Create;
  try
    for C := 'A' to 'Z' do
      for Y := 1 to 4096 do
        S.Write(C,SizeOf(C));

    for X := 1 to 16 do
      begin
        F.ImportFile(S,'test data '+IntToStr(X));
        H[X] := F.OpenFile('test data '+IntToStr(X),fkStream) as TStreamFileHandle;
      end;

    for X := 1 to 50 do
      for Y := 1 to 16 do
        H[Y].Write(S.Memory^,S.Size);

    for Y := 1 to 16 do
      H[Y].Destroy;
  finally
    S.Destroy;
  end;
  ReloadListView;
end;

procedure TFileContainerForm.OnCreateTestFile(Sender: TObject);
begin
  F.Active := False;
  DeleteFile('container.dat');
  F.Filename := 'container.dat';
  F.Open;
  UpdateControls;
end;

procedure TFileContainerForm.OnHexEdit(Sender: TObject);
var
  Selected: TListItem;
  Stream: TStream;
begin
  Selected := ListView.Selected;
  Stream := TMemoryStream.Create;
  try
    F.ExportFile(Stream,Selected.Caption);
    HexEdit(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TFileContainerForm.OnMap(Sender: TObject);
begin
  ShowPageMap(F);
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
  UpdateControls;
end;

procedure TFileContainerForm.OnExit(Sender: TObject);
begin
  if F.Active then
    OnClose(Sender);
  Close;
end;

procedure TFileContainerForm.OnExport(Sender: TObject);
var
  S: TFileStream;
  LI: TListItem;
  FI: TFile;
begin
  LI := ListView.Selected;
  FI := GetFile(LI);
  ExportDialog.Filename := FI.Name;
  if ExportDialog.Execute then
    begin
      ProgressForm.Caption := 'Exporting ' + FI.Name;
      ProgressForm.CaptionString := 'Exported %d of %d';
      S := TFileStream.Create(ExportDialog.Filename,fmCreate);
      try
        F.ExportFile(S,FI.Name);
      finally
        S.Free;
      end;
    end;
  UpdateControls;
end;

procedure TFileContainerForm.OnImport(Sender: TObject);
var
  S: TFileStream;
  LI: TListItem;
  FI: TFile;
  Filename: String;
begin
  if ImportDialog.Execute then
    begin
      S := TFileStream.Create(ImportDialog.Filename,fmOpenRead);
      try
        Filename := ExtractFilename(ImportDialog.Filename);
        ProgressForm.Caption := 'Importing ' + Filename;
        ProgressForm.CaptionString := 'Imported %d of %d';
        F.ImportFile(S,Filename);
        FI := F.Files.Find(Filename);
        Assert(FI<>nil);
        LI := ListView.Items.Add;
        LI.Caption := Filename;
        LI.SubItems.Add(IntToStr(FI.Size));
        LI.SubItems.Add(GetKindName(FI.Kind));
        LI.SubItems.Add(IntToStr(FI.PageCount));
        LI.SubItems.Add(IntToStr(FI.RecordCount));
        LI.SubItems.Add(IntToStr(FI.RootNode));
        LI.SubItems.Add(IntToStr(FI.ID));
      finally
        S.Free;
      end;
    end;
  UpdateControls;
  UpdatePageMap;
end;

end.

