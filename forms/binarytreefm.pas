unit binarytreefm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ComCtrls, Menus, rbtree;

type
  TRBTreeData = record
    Key, Data: Integer;
  end;
  PRBTreeData = ^TRBTreeData;

  { TBinaryTreeForm }

  TBinaryTreeForm = class(TForm)
    Data1: TEdit;
    MainMenu1: TMainMenu;
    Memo: TMemo;
    MenuItem1: TMenuItem;
    LoadTestItm: TMenuItem;
    RandomActionsItm: TMenuItem;
    SimulatedUseItm: TMenuItem;
    Rem100To50Itm: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    RandomDeleteItm: TMenuItem;
    RandomSearchItm: TMenuItem;
    RandomInsertItm: TMenuItem;
    SequentialInsertItm: TMenuItem;
    Add100to50Itm: TMenuItem;
    MenuItem5: TMenuItem;
    Rem0to50Itm: TMenuItem;
    Add0to50Itm: TMenuItem;
    Next0: TEdit;
    Next1: TEdit;
    RefreshBtn: TButton;
    InsertBtn: TButton;
    RemoveBtn: TButton;
    SearchBtn: TButton;
    SpinEdit: TSpinEdit;
    StatusBar: TStatusBar;
    Tree: TTreeView;
    procedure Add0to50ItmClick(Sender: TObject);
    procedure Add100To50ItmClick(Sender: TObject);
    procedure DataEditEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
    procedure LoadTestItmClick(Sender: TObject);
    procedure RandomActionsItmClick(Sender: TObject);
    procedure RandomDeleteItmClick(Sender: TObject);
    procedure RandomInsertItmClick(Sender: TObject);
    procedure RandomSearchItmClick(Sender: TObject);
    procedure Rem0to50ItmClick(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure Rem100to50ItmClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
    procedure SequentialInsertItmClick(Sender: TObject);
    procedure SimulatedUseItmClick(Sender: TObject);
    procedure TreeSelectionChanged(Sender: TObject);
  private
    procedure AddTreeChildren(ParentNode: ComCtrls.TTreeNode; BNode: PTreeNode);
    { private declarations }
  public
    procedure Iterate(Data: Pointer);
  end;

var
  BinaryTreeForm: TBinaryTreeForm;
  Last: Integer;
  B: TRBTree;

implementation

uses
  DateUtils;

{$R *.lfm}

function CompareFunc(Data1, Data2: Pointer): Integer;
begin
  if PRBTreeData(Data1)^.Key > PRBTreeData(Data2)^.Key then
    Result := 1
  else
    if PRBTreeData(Data1)^.Key < PRBTreeData(Data2)^.Key then
      Result := -1
    else
      Result := 0;
end;

function AddrToStr(P: Pointer): String;
begin
  Result := Format('%p',[P]);
  Result := copy(Result,Length(Result)-8,8);
end;

{ TBinaryTreeForm }

procedure TBinaryTreeForm.FormCreate(Sender: TObject);
begin
  B := TRBTree.Create(@CompareFunc,SizeOf(TRBTreeData));
  Randomize;
end;

procedure TBinaryTreeForm.FormDestroy(Sender: TObject);
begin
  B.Free;
end;

procedure TBinaryTreeForm.InsertBtnClick(Sender: TObject);
var
  R: TRBTreeData;
begin
  R.Key := SpinEdit.Value;
  R.Data := R.Key;
  if B.Insert(@R) then
    StatusBar.SimpleText := 'Key '+IntToStr(SpinEdit.Value)+' inserted'
  else
    StatusBar.SimpleText := 'Duplicate key '+IntToStr(SpinEdit.Value)+' found';
  RefreshBtnClick(nil);
end;

procedure TBinaryTreeForm.LoadTestItmClick(Sender: TObject);
var
  x: Integer;
begin
  for X := 1 to 100 do
    begin
      Memo.Lines.Clear;
      SimulatedUseItmClick(nil);
      Application.ProcessMessages;
    end;
end;

procedure TBinaryTreeForm.RandomActionsItmClick(Sender: TObject);
var
  X: Integer;
  P: Pointer;
  R: TRBTreeData;
begin
  //while true do begin
  for X := 1 to 10000000 do
    begin
      R.Key := Random(100);
      R.Data := R.Key;
      case Random(3) of
        0: B.Insert(@R);
        1: B.Remove(@R);
        2: begin
             if B.Search(@R) then
               if R.Key <> R.Data then raise Exception.Create('Data not consistent');
           end;
      end;
    end;
  Memo.Lines.Add('Complete');
  B.Clear;
  if B.CalcRecordCount > 0 then
    raise Exception.Create('CalcRecordCount');
 // end;
end;

procedure TBinaryTreeForm.RandomDeleteItmClick(Sender: TObject);
var
  Time1,Time2: TDateTime;
  ms: Int64;
  X: Integer;
  R: TRBTreeData;
begin
  B.Clear;
  for X := 1 to 1000000 do
    begin
      R.Key := Random(1000000);
      R.Data := R.Key;
      B.Insert(@R);
    end;
  Time1 := Now;
  for X := 1 to 1000000 do
    begin
      R.Key := Random(1000000);
      B.Remove(@R);
    end;
  Time2 := Now;
  ms := MillisecondsBetween(Time2,Time1);
  Memo.Lines.Add('Random Delete: '+IntToStr(ms)+'ms');
  B.Clear;
end;

procedure TBinaryTreeForm.RandomInsertItmClick(Sender: TObject);
var
  Time1,Time2: TDateTime;
  ms: Int64;
  X: Integer;
  R: TRBTreeData;
begin
  B.Clear;
  Time1 := Now;
  for X := 1 to 1000000 do
    begin
      R.Key := Random(10000000);
      R.Data := R.Key;
      B.Insert(@R);
    end;
  Time2 := Now;
  ms := MillisecondsBetween(Time2,Time1);
  Memo.Lines.Add('Random Insert: '+IntToStr(ms)+'ms');
  B.Clear;
end;

procedure TBinaryTreeForm.RandomSearchItmClick(Sender: TObject);
var
  Time1,Time2: TDateTime;
  ms: Int64;
  D: Pointer;
  X: Integer;
  R: TRBTreeData;
begin
  B.Clear;
  for X := 1 to 1000000 do
    begin
      R.Key := X;
      R.Data := X;
      B.Insert(@R);
    end;
  Time1 := Now;
  for X := 1 to 1000000 do
    begin
      R.Key := Random(1000000) + 1;
      B.Search(@R);
      Assert(R.Key=R.Data);
    end;
  Time2 := Now;
  ms := MillisecondsBetween(Time2,Time1);
  Memo.Lines.Add('Random Search: '+IntToStr(ms)+'ms');
  B.Clear;
end;

procedure TBinaryTreeForm.Rem0to50ItmClick(Sender: TObject);
var
  X: Integer;
  R: TRBTreeData;
begin
  for X := 0 to 50 do
    begin
      R.Key := X;
      B.Remove(@R);
    end;
  RefreshBtnClick(nil);
end;

procedure TBinaryTreeForm.Add100To50ItmClick(Sender: TObject);
var
  X: Integer;
  R: TRBTreeData;
begin
  for X := 100 downto 50 do
    begin
      R.Key := X;
      R.Data := X;
      B.Insert(@R);
    end;
  RefreshBtnClick(nil);
end;

procedure TBinaryTreeForm.Add0to50ItmClick(Sender: TObject);
var
  X: Integer;
  R: TRBTreeData;
begin
  for X := 0 to 50 do
    begin
      R.Key := X;
      R.Data := X;
      B.Insert(@R);
    end;
  RefreshBtnClick(nil);
end;

procedure TBinaryTreeForm.DataEditEnter(Sender: TObject);
var
  E: TEdit;
begin
  E := TEdit(Sender);
  if Assigned(E) then
    try
      SpinEdit.Value := StrToInt(E.Text);
    except
    end;
end;

procedure TBinaryTreeForm.Iterate(Data: Pointer);
begin
  Memo.Lines.Add(Format('%d:%d',[PRBTreeData(Data)^.Key,PRBTreeData(Data)^.Data]));
  if PRBTreeData(Data)^.Key <> PRBTreeData(Data)^.Data then
    raise Exception.Create('Data not associated with proper key');
  if PRBTreeData(Data)^.Key < Last then
    raise Exception.Create('Out of order');
  Last := PRBTreeData(Data)^.Key;
end;

procedure TBinaryTreeForm.RefreshBtnClick(Sender: TObject);
var
  N: ComCtrls.TTreeNode;
begin
  Tree.Items.Clear;
  if B.FRoot <> nil then
    begin
      N := Tree.Items.AddObject(nil,AddrToStr(B.FRoot),B.FRoot);
      AddTreeChildren(N,B.FRoot);
    end;
end;

procedure TBinaryTreeForm.Rem100to50ItmClick(Sender: TObject);
var
  X: Integer;
  R: TRBTreeData;
begin
  for X := 100 downto 50 do
    begin
      R.Key := X;
      B.Remove(@R);
    end;
  RefreshBtnClick(nil);
end;

procedure TBinaryTreeForm.RemoveBtnClick(Sender: TObject);
var
  R: TRBTreeData;
begin
  R.Key := SpinEdit.Value;
  if B.Remove(@R) then
    StatusBar.SimpleText := 'Key '+IntToStr(SpinEdit.Value)+' deleted'
  else
    StatusBar.SimpleText := 'Key '+IntToStr(SpinEdit.Value)+' not found';
  RefreshBtnClick(nil);
end;

procedure TBinaryTreeForm.SearchBtnClick(Sender: TObject);
var
  R: TRBTreeData;
begin
  R.Key := SpinEdit.Value;
  if B.Search(@R) then
    StatusBar.SimpleText := 'Key '+IntToStr(SpinEdit.Value)+' found'
  else
    StatusBar.SimpleText := 'Key '+IntToStr(SpinEdit.Value)+' not found';
end;

procedure TBinaryTreeForm.SequentialInsertItmClick(Sender: TObject);
var
  Time1,Time2: TDateTime;
  ms: Int64;
  X: Integer;
  R: TRBTreeData;
begin
  B.Clear;
  Time1 := Now;
  for X := 1 to 1000000 do
    begin
      R.Key := X;
      R.Data := X;
      B.Insert(@R);
    end;
  Time2 := Now;
  ms := MillisecondsBetween(Time2,Time1);
  Memo.Lines.Add('Sequential Insert: '+IntToStr(ms)+'ms');
  B.Clear;
end;

procedure TBinaryTreeForm.SimulatedUseItmClick(Sender: TObject);
var
  I,J: Integer;
  D: Pointer ;
  R: TRBTreeData;
begin
  B.Clear;
  for I := 1 to 10000 do
    begin
      R.Key := Random(9000);
      R.Data := R.Key;
      B.Insert(@R);
    end;
  Assert(B.RecordCount=B.CalcRecordCount);

  for I := 1 to 5000 do
    begin
      R.Key := Random(9000);
      B.Remove(@R);
    end;
  Last := -MaxInt;
  B.Iterate(@Iterate);
  Assert(B.RecordCount=B.CalcRecordCount);

  for I := 1 to 2500 do
    begin
      R.Key := Random(9000);
      R.Data := R.Key;
      B.Insert(@R);
    end;

  Assert(B.RecordCount=B.CalcRecordCount);

  for I := 1 to 2500 do
    begin
      R.Key := Random(9000);
      B.Remove(@R);
    end;
  Last := -MaxInt;
  B.Iterate(@Iterate);
  Assert(B.RecordCount=B.CalcRecordCount);

  for I := 1 to 1000 do
    begin
      R.Key := Random(9000);
      B.Search(@R);
      Assert(R.Key=R.Data);
    end;
  Last := -MaxInt;

  Memo.Lines.Add(IntToStr(B.RecordCount)+' records');
  B.Clear;
end;

procedure TBinaryTreeForm.AddTreeChildren(ParentNode: ComCtrls.TTreeNode; BNode: PTreeNode);
var
  P: PTreeNode;
  N: ComCtrls.TTreeNode;
  I: Integer;
begin
  if BNode = nil then Exit;
  N := Tree.Items.AddChildObject(ParentNode,AddrToStr(BNode^.Left),BNode^.Left);
  AddTreeChildren(N,BNode^.Left);
  N := Tree.Items.AddChildObject(ParentNode,AddrToStr(BNode^.Right),BNode^.Right);
  AddTreeChildren(N,BNode^.Right);
end;

procedure TBinaryTreeForm.TreeSelectionChanged(Sender: TObject);
var
  N: PTreeNode;
begin
  if Assigned(Tree.Selected) then
    begin
      N := PTreeNode(Tree.Selected.Data);
      if Assigned(N) then
        begin
          Data1.Text := IntToStr(PRBTreeData(N^.Data)^.Key);
          Next0.Text := AddrToStr(N^.Left);
          Next1.Text := AddrToStr(N^.Right);
          if N^.Color = ncRed then
            begin
              Data1.Font.Color := clMaroon;
              Next0.Font.Color := clMaroon;
              Next1.Font.Color := clMaroon;
            end
          else
            begin
              Data1.Font.Color := clBlack;
              Next0.Font.Color := clBlack;
              Next1.Font.Color := clBlack;
            end;
        end;
    end;
end;

end.

