unit securityfm;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, files, Crypto;

type

  { TSecurityDlg }

  TSecurityDlg = class(TForm)
    AlgorithmCombo: TComboBox;
    ChangePasswordBtn: TButton;
    DecryptBtn: TButton;
    DefragBtn: TButton;
    EncryptBtn: TButton;
    LBAlgorithm: TLabel;
    LBPassword: TLabel;
    PackBtn: TButton;
    CloseBtn: TButton;
    Password: TEdit;
    SetPasswordBtn: TButton;
    procedure AlgorithmComboChange(Sender: TObject);
    procedure ChangePasswordBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure DecryptBtnClick(Sender: TObject);
    procedure DefragBtnClick(Sender: TObject);
    procedure EncryptBtnClick(Sender: TObject);
    procedure PackBtnClick(Sender: TObject);
    procedure PasswordChange(Sender: TObject);
    procedure SetPasswordBtnClick(Sender: TObject);
  private
    FContainer: TFileContainer;
    FHeader: TFileHeader;
    FAlg: TCipherAlgorithm;
    function SelectedAlgorithm: TCipherAlgorithm;
    procedure UpdateEncryptorNames;
    procedure UpdateSecurityTab;
  public
    procedure UpdateAll;
  end;

var
  SecurityDlg: TSecurityDlg;

procedure ShowSecurityDlg(Container: TFileContainer);

implementation

{$R *.lfm}

type
  TFileContainerHack = class(TFileContainer);

procedure ShowSecurityDlg(Container: TFileContainer);
begin
  if not Container.Active then Exit;
  if not Assigned(SecurityDlg) then
    SecurityDlg := TSecurityDlg.Create(Application);
  SecurityDlg.FContainer := Container;
  SecurityDlg.FHeader := TFileContainerHack(SecurityDlg.FContainer).Header;
  SecurityDlg.UpdateAll;
  SecurityDlg.Show;
end;

{ TSecurityDlg }

procedure TSecurityDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TSecurityDlg.UpdateSecurityTab;
begin
  ChangePasswordBtn.Enabled := FContainer.Encrypted and (Length(Password.Text) > 3);
  EncryptBtn.Visible := (not FContainer.Encrypted);
  EncryptBtn.Enabled := Length(Password.Text) > 3;
  DecryptBtn.Visible := FContainer.Encrypted;
  AlgorithmCombo.Enabled := not FContainer.Encrypted;
end;

procedure TSecurityDlg.UpdateEncryptorNames;
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

procedure TSecurityDlg.UpdateAll;
begin
  UpdateSecurityTab;
  UpdateEncryptorNames;
end;

// SECURITY TAB

procedure TSecurityDlg.SetPasswordBtnClick(Sender: TObject);
begin
  FContainer.Password := Password.Text;
  UpdateSecurityTab;
end;

procedure TSecurityDlg.ChangePasswordBtnClick(Sender: TObject);
begin
  FContainer.Password := Password.Text;
  MessageDlg('Password was successfully changed',mtInformation,[mbClose],0);
  UpdateSecurityTab;
end;

procedure TSecurityDlg.PasswordChange(Sender: TObject);
begin
  UpdateSecurityTab;
end;

function TSecurityDlg.SelectedAlgorithm: TCipherAlgorithm;
var
  I: PtrInt;
begin
  I := PtrInt(AlgorithmCombo.Items.Objects[AlgorithmCombo.ItemIndex]);
  Result := TCipherAlgorithm(I);
end;

procedure TSecurityDlg.AlgorithmComboChange(Sender: TObject);
begin
  FAlg := SelectedAlgorithm;
end;

procedure TSecurityDlg.EncryptBtnClick(Sender: TObject);
begin
  Assert(not FContainer.Encrypted);
  FContainer.Encrypt(Password.Text,SelectedAlgorithm);
  MessageDlg('Filestore has been encrypted',mtInformation,[mbClose],0);
  UpdateSecurityTab;
end;

procedure TSecurityDlg.DecryptBtnClick(Sender: TObject);
begin
  Assert(FContainer.Encrypted);
  FContainer.Decrypt;
  MessageDlg('Filestore has been decrypted',mtInformation,[mbClose],0);
  UpdateSecurityTab;
end;

procedure TSecurityDlg.DefragBtnClick(Sender: TObject);
begin
  FContainer.Defrag;
  MessageDlg('Filestore defragmentation complete',mtInformation,[mbClose],0);
end;

procedure TSecurityDlg.PackBtnClick(Sender: TObject);
begin
  FContainer.Pack(255);
  MessageDlg('Filestore has been packed',mtInformation,[mbClose],0);
end;

end.

