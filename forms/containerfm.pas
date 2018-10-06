unit ContainerFM;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ComCtrls, ExtCtrls, files, Crypto;

type

  { TContainerPropertiesDlg }

  TContainerPropertiesDlg = class(TForm)
    ClearPasswordBtn: TButton;
    AlgorithmCombo: TComboBox;
    DecryptBtn: TButton;
    EncryptBtn: TButton;
    ChangePasswordBtn: TButton;
    DefragBtn: TButton;
    LBAlgorithm: TLabel;
    PackBtn: TButton;
    FilesGrid: TStringGrid;
    FreePages: TStringGrid;
    LBFilename: TLabel;
    Filename: TLabel;
    Version: TLabel;
    FileSize: TLabel;
    PageCount: TLabel;
    FreePageCount: TLabel;
    MetadataSize: TLabel;
    MetadataPageCount: TLabel;
    FileCount: TLabel;
    LBFileSize: TLabel;
    LBPageCount: TLabel;
    LBVersion: TLabel;
    LBFreePageCount: TLabel;
    LBMetadataSize: TLabel;
    LBMetadataPageCount: TLabel;
    LBFileCount: TLabel;
    CloseBtn: TButton;
    LBPassword: TLabel;
    MetaPages: TStringGrid;
    PageControl: TPageControl;
    Password: TEdit;
    SetPasswordBtn: TButton;
    GeneralTab: TTabSheet;
    SecurityTab: TTabSheet;
    FreePagesTab: TTabSheet;
    MetadataTab: TTabSheet;
    FilesTab: TTabSheet;
    procedure AlgorithmComboChange(Sender: TObject);
    procedure ChangePasswordBtnClick(Sender: TObject);
    procedure ClearPasswordBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure DecryptBtnClick(Sender: TObject);
    procedure DefragBtnClick(Sender: TObject);
    procedure EncryptBtnClick(Sender: TObject);
    procedure FilesGridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure PackBtnClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PasswordChange(Sender: TObject);
    procedure SetPasswordBtnClick(Sender: TObject);
  private
    FContainer: TFileContainer;
    FHeader: TFileHeader;
    FAlg: TCipherAlgorithm;
    function FileKindName(Kind: TFileKind): String;
    function SelectedAlgorithm: TCipherAlgorithm;
    procedure UpdateEncryptorNames;
    procedure UpdateGeneralTab;
    procedure UpdateSecurityTab;
    procedure UpdateFilesTab;
    procedure UpdateFreePagesTab;
    procedure UpdateMetadataTab;
  public
    procedure UpdateAll;
  end;

var
  ContainerPropertiesDlg: TContainerPropertiesDlg;

procedure ShowContainerProperties(Container: TFileContainer);

implementation

{$R *.lfm}

type
  TFileContainerHack = class(TFileContainer);

procedure ShowContainerProperties(Container: TFileContainer);
begin
  if not Assigned(ContainerPropertiesDlg) then
    ContainerPropertiesDlg := TContainerPropertiesDlg.Create(Application);
  ContainerPropertiesDlg.FContainer := Container;
  ContainerPropertiesDlg.FHeader := TFileContainerHack(ContainerPropertiesDlg.FContainer).Header;
  ContainerPropertiesDlg.UpdateAll;
  ContainerPropertiesDlg.Show;
end;

{ TContainerPropertiesDlg }


procedure TContainerPropertiesDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TContainerPropertiesDlg.UpdateEncryptorNames;
var
  Alg: TCipherAlgorithm;
  I: PtrInt;
begin
  AlgorithmCombo.Items.Clear;
  for Alg := Low(Alg) to High(Alg) do
    if not (Alg in [caDES]) then
      begin
        I := ord(Alg);
        AlgorithmCombo.Items.AddObject(CIPHER_ALGORITHM_STR[Alg],TObject(I));
      end;
  I := ord(FContainer.CipherAlgorithm);
  AlgorithmCombo.ItemIndex := AlgorithmCombo.Items.IndexOfObject(TObject(I));
  FAlg := FContainer.CipherAlgorithm;
end;

procedure TContainerPropertiesDlg.UpdateAll;
begin
  UpdateGeneralTab;
  UpdateSecurityTab;
  UpdateEncryptorNames;
  UpdateFreePagesTab;
  UpdateMetadataTab;
  UpdateFilesTab;
end;

procedure TContainerPropertiesDlg.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = GeneralTab then UpdateGeneralTab;
  if PageControl.ActivePage = SecurityTab then UpdateSecurityTab;
  if PageControl.ActivePage = FreePagesTab then UpdateFreePagesTab;
  if PageControl.ActivePage = MetadataTab then UpdateMetadataTab;
  if PageControl.ActivePage = FilesTab then UpdateFilesTab;
end;

// GENERAL TAB

procedure TContainerPropertiesDlg.UpdateGeneralTab;
begin
  Filename.Caption := FContainer.Filename;
  Version.Caption := IntToStr(FContainer.Version);
  FileSize.Caption := IntToStr(FContainer.PageCount * PAGE_SIZE);
  PageCount.Caption := IntToStr(FContainer.PageCount);
  FreePageCount.Caption := IntToStr(FContainer.FreePages.Count);
  MetadataSize.Caption := IntToStr(FContainer.Metadata.Size);
  MetadataPageCount.Caption := IntToStr(FContainer.Metadata.PageCount);
  FileCount.Caption := IntToStr(FContainer.Files.Count);
end;

// SECURITY TAB

procedure TContainerPropertiesDlg.SetPasswordBtnClick(Sender: TObject);
begin
  FContainer.Password := Password.Text;
  UpdateSecurityTab;
end;

procedure TContainerPropertiesDlg.ChangePasswordBtnClick(Sender: TObject);
begin
  FContainer.Password := Password.Text;
  UpdateSecurityTab;
end;

function TContainerPropertiesDlg.SelectedAlgorithm: TCipherAlgorithm;
var
  I: PtrInt;
begin
  I := PtrInt(AlgorithmCombo.Items.Objects[AlgorithmCombo.ItemIndex]);
  Result := TCipherAlgorithm(I);
end;

procedure TContainerPropertiesDlg.AlgorithmComboChange(Sender: TObject);
begin
  FAlg := SelectedAlgorithm;
end;

procedure TContainerPropertiesDlg.ClearPasswordBtnClick(Sender: TObject);
begin
  Assert(not FContainer.Encrypted);
  FContainer.Password := '';
  Password.Text := '';
  UpdateSecurityTab;
end;

procedure TContainerPropertiesDlg.PasswordChange(Sender: TObject);
begin
  UpdateSecurityTab;
end;

procedure TContainerPropertiesDlg.DecryptBtnClick(Sender: TObject);
begin
  Assert(FContainer.Encrypted);
  FContainer.Decrypt;
  UpdateSecurityTab;
end;

procedure TContainerPropertiesDlg.DefragBtnClick(Sender: TObject);
begin
  FContainer.Defrag;
end;

procedure TContainerPropertiesDlg.EncryptBtnClick(Sender: TObject);
begin
  Assert(not FContainer.Encrypted);
  FContainer.Encrypt(Password.Text,SelectedAlgorithm);
  UpdateSecurityTab;
end;

procedure TContainerPropertiesDlg.UpdateSecurityTab;
begin
  ChangePasswordBtn.Enabled := (Password.Text > '');
  ClearPasswordBtn.Enabled := True;
  SetPasswordBtn.Enabled := True;
  EncryptBtn.Enabled := (not FContainer.Encrypted);
  DecryptBtn.Enabled := FContainer.Encrypted;
  Password.Enabled := True;
  AlgorithmCombo.Enabled := not FContainer.Encrypted;
end;

// FREE PAGES

procedure TContainerPropertiesDlg.UpdateFreePagesTab;
var
  I: Integer;
begin
  FreePages.Clear;
  FreePages.RowCount := MAX_FREE_PAGES + 1;
  for I := 1 to MAX_FREE_PAGES do
    begin
      FreePages.Cells[0,I] := IntToStr(I);
      FreePages.Cells[1,I] := IntToStr(FHeader.FreePages[I]);
    end;
end;

// METADATA TAB

procedure TContainerPropertiesDlg.UpdateMetadataTab;
var
  I: Integer;
begin
  MetaPages.Clear;
  MetaPages.RowCount := MAX_META_PAGES + 1;
  for I := 1 to MAX_META_PAGES do
    begin
      MetaPages.Cells[0,I] := IntToStr(I);
      MetaPages.Cells[1,I] := IntToStr(FHeader.MetaPages[I]);
    end;
end;

// FILES TAB

procedure TContainerPropertiesDlg.FilesGridButtonClick(Sender: TObject; aCol, aRow: Integer);
var
  I: Integer;
  F: TFile;
begin
  I := aRow - 1;
  F := FContainer.Files[I];
  Assert(Assigned(F));
  //ShowFilePropertiesDlg(FContainer,F.ID);
end;

procedure TContainerPropertiesDlg.PackBtnClick(Sender: TObject);
begin
  FContainer.Pack(255);
end;

function TContainerPropertiesDlg.FileKindName(Kind: TFileKind): String;
begin
  case Kind of
    fkStream : Result := 'Stream';
    fkPage   : Result := 'Page';
    fkBTree  : Result := 'BTree';
    fkTable  : Result := 'Table';
  end;
end;

procedure TContainerPropertiesDlg.UpdateFilesTab;
var
  I: Integer;
  C: Integer;
  F: TFile;
begin
  FilesGrid.Clear;
  C := FContainer.Files.Count;
  FilesGrid.RowCount := C + 1;
  for I := 1 to C do
    begin
      F := FContainer.Files[I-1];
      Assert(Assigned(F));
      FilesGrid.Cells[0,I] := F.Name;
      FilesGrid.Cells[1,I] := IntToStr(F.Size);
      FilesGrid.Cells[2,I] := FileKindName(F.Kind);
    end;
end;

end.
