unit MainFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ComCtrls, Menus, btree3, Files, BTreeTestHelper;

type

  { TMain }

  TMain = class(TForm)
    Label5: TLabel;
    Log: TMemo;
    Add1To10Itm: TMenuItem;
    DeleteFileContainerItm: TMenuItem;
    DumpBufferRefCountsItm: TMenuItem;
    Add10To1Itm: TMenuItem;
    Add500To0Itm: TMenuItem;
    Add500To1000Itm: TMenuItem;
    DumpFATItm: TMenuItem;
    FindFailureSequence: TMenuItem;
    MenuItem2: TMenuItem;
    InsertSeqItm: TMenuItem;
    MenuItem3: TMenuItem;
    RemoveSeqItm: TMenuItem;
    SearchSeqItm: TMenuItem;
    NewRandomSequenceItm: TMenuItem;
    RandomTestItm: TMenuItem;
    Remove500To0Itm: TMenuItem;
    Remove500To1000Itm: TMenuItem;
    OperationsMenu: TMenuItem;
    FileSize: TEdit;
    ViewMenu: TMenuItem;
    PageCount: TEdit;
    ClearBtn: TButton;
    IterateBtn: TButton;
    Data1: TEdit;
    Data5: TEdit;
    Count: TEdit;
    Data6: TEdit;
    Add1To6Itm: TMenuItem;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    NodeCount: TEdit;
    Label1: TLabel;
    LBCount: TLabel;
    MainMenu1: TMainMenu;
    Memo: TMemo;
    MenuItem1: TMenuItem;
    LoadTestItm: TMenuItem;
    Next6: TEdit;
    RecordCount: TEdit;
    BufferCount: TEdit;
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
    Next5: TEdit;
    Next0: TEdit;
    Next1: TEdit;
    Data2: TEdit;
    Next2: TEdit;
    Data3: TEdit;
    Next3: TEdit;
    Data4: TEdit;
    Next4: TEdit;
    RefreshBtn: TButton;
    InsertBtn: TButton;
    RemoveBtn: TButton;
    SearchBtn: TButton;
    SpinEdit: TSpinEdit;
    StatusBar: TStatusBar;
    Tree: TTreeView;
    procedure Add0to50ItmClick(Sender: TObject);
    procedure Add100To50ItmClick(Sender: TObject);
    procedure Add10To1ItmClick(Sender: TObject);
    procedure Add1To10ItmClick(Sender: TObject);
    procedure Add1To6ItmClick(Sender: TObject);
    procedure Add500To0ItmClick(Sender: TObject);
    procedure Add500To1000ItmClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure DataEditEnter(Sender: TObject);
    procedure DeleteFileContainerItmClick(Sender: TObject);
    procedure DumpBufferRefCountsItmClick(Sender: TObject);
    procedure DumpFATItmClick(Sender: TObject);
    procedure FindFailureSequenceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
    procedure InsertSeqItmClick(Sender: TObject);
    procedure IterateBtnClick(Sender: TObject);
    procedure LoadTestItmClick(Sender: TObject);
    procedure NewRandomSequenceItmClick(Sender: TObject);
    procedure RandomDeleteItmClick(Sender: TObject);
    procedure RandomInsertItmClick(Sender: TObject);
    procedure RandomSearchItmClick(Sender: TObject);
    procedure RandomTestItmClick(Sender: TObject);
    procedure Rem0to50ItmClick(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure Rem100to50ItmClick(Sender: TObject);
    procedure Remove500To0ItmClick(Sender: TObject);
    procedure Remove500To1000ItmClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure RemoveSeqItmClick(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
    procedure SearchSeqItmClick(Sender: TObject);
    procedure SequentialInsertItmClick(Sender: TObject);
    procedure SimulatedUseItmClick(Sender: TObject);
    procedure TreeSelectionChanged(Sender: TObject);
  private
    Last: Integer;
    procedure AddTreeChildren(ParentNode: TTreeNode; BTreeNode: TBTreeNode);
    { private declarations }
  public
    procedure Iterate(Rec: Pointer);
    procedure HandleLog(Sender: TObject; Msg: String);
  end;

var
  Main: TMain;
  F: TFileContainer;
  B: TBTreeFileHandle;
  Helper: TBTreeTestHelper;
  Rand: TRandomArray;

implementation

uses
  DateUtils;

{$R *.lfm}

type
  TFileHack = class(TFile);
  TFileFATHack = class(TFileFAT);

function AddrToStr(P: Pointer): String;
begin
  Result := Format('%p',[P]);
  Result := copy(Result,Length(Result)-8,8);
end;

{ TMain }

procedure TMain.FormCreate(Sender: TObject);
begin
  DeleteFile('btree.dat');
  F := TFileContainer.Create;
  F.Filename := 'btree.dat';
  F.Active := True;
  B := F.CreateFile('btree',fkbTree) as TBTreeFileHandle;
  B.Init(SizeOf(TRec),@IntegerCompareFunc);
  B.Order := 6;
  Randomize;
  F.OnLog := @HandleLog;
  Helper := TBTreeTestHelper.Create(B);
  Rand := TRandomArray.Create;
  Rand.LoadState;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  B.Destroy;
  F.Destroy;
  //Rand.SaveState;
  Rand.Destroy;
  Helper.Free;
end;

procedure TMain.InsertBtnClick(Sender: TObject);
var
  R: TRec;
begin
  R.Key := SpinEdit.Value;
  R.Data := R.Key;
  if B.Insert(@R) then
    StatusBar.SimpleText := 'Key '+IntToStr(SpinEdit.Value)+' inserted'
  else
    StatusBar.SimpleText := 'Duplicate key '+IntToStr(SpinEdit.Value)+' found';
  RefreshBtnClick(nil);
end;

procedure TMain.InsertSeqItmClick(Sender: TObject);
begin
  Helper.InsertSeq(Rand);
  RefreshBtnClick(nil);
end;

procedure TMain.SearchBtnClick(Sender: TObject);
var
  Rec: TRec;
begin
  Rec.Key := SpinEdit.Value;
  if B.Search(@Rec) then
    StatusBar.SimpleText := 'Key '+IntToStr(SpinEdit.Value)+' found'
  else
    StatusBar.SimpleText := 'Key '+IntToStr(SpinEdit.Value)+' not found';
end;

procedure TMain.SearchSeqItmClick(Sender: TObject);
begin
  Helper.SearchSeq(Rand);
  RefreshBtnClick(nil);
end;

procedure TMain.RemoveBtnClick(Sender: TObject);
var
  V: Integer;
begin
  V := SpinEdit.Value;
  if B.Remove(@V) then
    StatusBar.SimpleText := 'Key '+IntToStr(SpinEdit.Value)+' deleted'
  else
    StatusBar.SimpleText := 'Key '+IntToStr(SpinEdit.Value)+' not found';
  RefreshBtnClick(nil);
end;

procedure TMain.RemoveSeqItmClick(Sender: TObject);
begin
  Helper.RemoveSeq(Rand);
  RefreshBtnClick(nil);
end;

procedure TMain.IterateBtnClick(Sender: TObject);
begin
  Last := -MaxInt;
  B.Iterate(@Iterate);
end;

procedure TMain.RandomDeleteItmClick(Sender: TObject);
var
  Time1,Time2: TDateTime;
  ms: Int64;
  X: Integer;
  R: TRec;
begin
  B.Clear;
  for X := 1 to 10000 do
    begin
      R.Key := X;
      R.Data := X;
      B.Insert(@R);
    end;
  Time1 := Now;
  for X := 1 to 10000 do
    begin
      R.Key := Random(10000)+1;
      B.Remove(@R);
    end;
  Time2 := Now;
  ms := MillisecondsBetween(Time2,Time1);
  Memo.Lines.Add('Random Delete: '+IntToStr(ms)+'ms');
  B.Clear;
end;

procedure TMain.RandomInsertItmClick(Sender: TObject);
var
  Time1,Time2: TDateTime;
  ms: Int64;
  X: Integer;
  R: TRec;
begin
  B.Clear;
  Time1 := Now;
  for X := 1 to 10000 do
    begin
      R.Key := Random(10000);
      R.Data := R.Key;
      B.Insert(@R);
    end;
  Time2 := Now;
  ms := MillisecondsBetween(Time2,Time1);
  Memo.Lines.Add('Random Insert: '+IntToStr(ms)+'ms');
  B.Clear;
end;

procedure TMain.RandomSearchItmClick(Sender: TObject);
var
  Time1,Time2: TDateTime;
  ms: Int64;
  X: Integer;
  R: TRec;
begin
  B.Clear;
  for X := 1 to 10000 do
    begin
      R.Key := X;
      R.Data := X;
      B.Insert(@R);
    end;
  Time1 := Now;
  for X := 1 to 10000 do
    begin
      R.Key := Random(10000)+1;
      R.Data := 0;
      B.Search(@R);
    end;
  Time2 := Now;
  ms := MillisecondsBetween(Time2,Time1);
  Memo.Lines.Add('Random Search: '+IntToStr(ms)+'ms');
  B.Clear;
end;

procedure TMain.RandomTestItmClick(Sender: TObject);
begin
  Helper.InsertSeq(Rand);
  Memo.Lines.Add('Insert Sequence Passed');
  Helper.SearchSeq(Rand);
  Memo.Lines.Add('Search Sequence Passed');
  B.Flush;
  Helper.SearchSeq(Rand);
  Memo.Lines.Add('Search Sequence Passed');
  //B.Clear;
  Helper.RemoveSeq(Rand);
  Memo.Lines.Add('Remove Sequence Passed');
end;

procedure TMain.Add100To50ItmClick(Sender: TObject);
begin
  Helper.Add(100,50);
  RefreshBtnClick(nil);
end;

procedure TMain.Add0to50ItmClick(Sender: TObject);
begin
  Helper.Add(0,50);
  RefreshBtnClick(nil);
end;

procedure TMain.Add10To1ItmClick(Sender: TObject);
begin
  Helper.Add(10,1);
  RefreshBtnClick(nil);
end;

procedure TMain.Add1To10ItmClick(Sender: TObject);
begin
  Helper.Add(1,10);
  RefreshBtnClick(nil);
end;

procedure TMain.Add1To6ItmClick(Sender: TObject);
begin
  Helper.Add(1,6);
  RefreshBtnClick(nil);
end;

procedure TMain.Add500To0ItmClick(Sender: TObject);
begin
  Helper.Add(500,0);
  RefreshBtnClick(nil);
end;

procedure TMain.Add500To1000ItmClick(Sender: TObject);
begin
  Helper.Add(500,1000);
  RefreshBtnClick(nil);
end;

procedure TMain.Rem0to50ItmClick(Sender: TObject);
begin
  Helper.Remove(0,50);
  RefreshBtnClick(nil);
end;

procedure TMain.Rem100to50ItmClick(Sender: TObject);
begin
  Helper.Remove(100,50);
  RefreshBtnClick(nil);
end;

procedure TMain.Remove500To0ItmClick(Sender: TObject);
begin
  Helper.Remove(500,0);
  RefreshBtnClick(nil);
end;

procedure TMain.Remove500To1000ItmClick(Sender: TObject);
begin
  Helper.Remove(500,1000);
  RefreshBtnClick(nil);
end;

procedure TMain.ClearBtnClick(Sender: TObject);
begin
  B.Clear;
  RefreshBtnClick(nil);
end;

procedure TMain.DataEditEnter(Sender: TObject);
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

procedure TMain.DeleteFileContainerItmClick(Sender: TObject);
begin
  B.Destroy;
  F.Free;
  Helper.Free;
  Sleep(100);
  DeleteFile('btree.dat');
  F := TFileContainer.Create;
  F.Filename := 'btree.dat';
  F.Active := True;
  B := F.CreateFile('btree',fkbTree) as TBTreeFileHandle;
  B.Init(SizeOf(TRec),@IntegerCompareFunc);
  B.Order := 6;
  Tree.Items.Clear;
  Helper := TBTreeTestHelper.Create(B);
  RefreshBtnClick(nil);
end;

procedure TMain.DumpBufferRefCountsItmClick(Sender: TObject);
var
  X: Integer;
  Bf: TFileBuffer;
begin
  Bf := TFileHack(B._File).Buffer;
  for X := 0 to Bf.Count - 1 do
    HandleLog(Self,Format('Buffer[%d]= {PageNum=%d,PageIdx=%d,RefCount=%d,Modified=%s}',[X,Bf[X]^.PageNum,Bf[X]^.PageIdx,Bf[X]^.RefCount,BoolToStr(Bf[X]^.Modified,True)]));
end;

procedure TMain.DumpFATItmClick(Sender: TObject);
var
  F: TFileFATHack;
  X: Integer;
begin
  Memo.Clear;
  F := TFileFATHack(TFileHack(B._File).FAT);
  for X := 0 to F.Count - 1 do
    Memo.Lines.Add('['+IntToStr(X)+']'+IntToStr(F.Items[X]));
end;

procedure TMain.FindFailureSequenceClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to 1000 do
    begin
      F.LogFmt('FindFailureSequence Iteration %d',[I]);
      NewRandomSequenceItmClick(nil);
      RandomTestItmClick(nil);
      Application.ProcessMessages;
    end;
end;

procedure TMain.Iterate(Rec: Pointer);
begin
  Memo.Lines.Add(Format('%d:%d',[PRec(Rec)^.Key,PRec(Rec)^.Data]));
  if PRec(Rec)^.Data <> PRec(Rec)^.Key then
    raise Exception.Create('Data not associated with proper key');
  if PRec(Rec)^.Key < Self.Last then
    raise Exception.Create('Out of order');
    //ShowMessage('Out of order');
  Self.Last := PRec(Rec)^.Key;
end;

procedure TMain.HandleLog(Sender: TObject; Msg: String);
begin
  Log.Lines.Add(Msg);
end;

procedure TMain.RefreshBtnClick(Sender: TObject);
var
  V: PtrInt;
  N: TTreeNode;
begin
  Tree.Items.Clear;
  if Assigned(B.Root) then
    begin
      V := B.Root.Page^.PageIdx + 1;
      N := Tree.Items.AddObject(nil,IntToStr(B.Root.Page^.PageIdx+1),Pointer(V));
      AddTreeChildren(N,B.Root);
    end;
  NodeCount.Caption := IntToStr(B.NodeCount);
  RecordCount.Caption := IntToStr(B.RecordCount);
  BufferCount.Caption := IntToStr(TFileHack(B._File).Buffer.Count);
  PageCount.Caption := IntToStr(B.PageCount);
  FileSize.Caption := IntToStr(B._File.Size);
end;

procedure TMain.SequentialInsertItmClick(Sender: TObject);
var
  Time1,Time2: TDateTime;
  ms: Int64;
  X: Integer;
  R: TRec;
begin
  B.Clear;
  Time1 := Now;
  for X := 1 to 10000 do
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

procedure TMain.SimulatedUseItmClick(Sender: TObject);
var
  I,J: NativeInt;
  R: TRec;
  Found: Boolean;
begin
  for I := 1 to 1000 do
    begin
      J := Random(900);
      R.Key := J;
      R.Data := J;
      B.Insert(@R);
    end;
  Assert(B.RecordCount=B.CalcRecordCount);
  Last := -MaxInt;
  B.Iterate(@Iterate);
  Application.ProcessMessages;

  for I := 1 to 500 do
    begin
      R.Key := Random(900);
      B.Remove(@R);
    end;
  Last := -MaxInt;
  B.Iterate(@Iterate);
  Assert(B.RecordCount=B.CalcRecordCount);
  Application.ProcessMessages;

  for I := 1 to 250 do
    begin
      J := Random(900);
      R.Key := J;
      R.Data := J;
      B.Insert(@R);
    end;
  Last := -MaxInt;
  B.Iterate(@Iterate);
  Assert(B.RecordCount=B.CalcRecordCount);
  Application.ProcessMessages;

  for I := 1 to 250 do
    begin
      R.Key := Random(900);
      B.Remove(@R);
    end;
  Last := -MaxInt;
  B.Iterate(@Iterate);
  Assert(B.RecordCount=B.CalcRecordCount);
  Application.ProcessMessages;

  for I := 1 to 100 do
    begin
      R.Key := Random(900);
      B.Search(@R);
    end;
  Last := -MaxInt;
  B.Iterate(@Iterate);
  Assert(B.RecordCount=B.CalcRecordCount);
  RefreshBtnClick(nil);
end;

procedure TMain.LoadTestItmClick(Sender: TObject);
var
  X: Integer;
begin
  for X := 1 to 100 do
    begin
      Memo.Lines.Clear;
      SimulatedUseItmClick(nil);
      Application.ProcessMessages;
    end;
end;

procedure TMain.NewRandomSequenceItmClick(Sender: TObject);
begin
  Rand.Count := 1000;
  Rand.FillRandom;
  Rand.SaveState;
end;

procedure TMain.AddTreeChildren(ParentNode: TTreeNode; BTreeNode: TBTreeNode);
var
  P: TBTreeNode;
  N: TTreeNode;
  I: Integer;
  V: PtrInt;
  C: Integer;
begin
  if not Assigned(BTreeNode) then
    Exit;
  C := BTreeNode.Count;
  for I := 0 to C do
    begin
      P := BTreeNode.Next[I];
      if Assigned(P) then
        begin
          V := P.Page^.PageIdx+1;
          N := Tree.Items.AddChildObject(ParentNode,IntToStr(P.Page^.PageIdx+1),Pointer(V));
          AddTreeChildren(N,P);
          P.Destroy;
        end;
    end;
end;

procedure TMain.TreeSelectionChanged(Sender: TObject);
var
  N: TBTreeNode;
  V: PtrInt;
begin
  if Assigned(Tree.Selected) then
    begin
      if Tree.Selected.Parent = nil then
        N := B.Root
      else
        begin
          V := PtrInt(Tree.Selected.Data);
          //P := H.Fetch(V-1);
          N := TBTreeNode.Create(B,V-1);
        end;
      if Assigned(N) then
        begin
          //N.FTree := B;
          Data1.Text := IntToStr(PInteger(N.Data[1])^);
          Data2.Text := IntToStr(PInteger(N.Data[2])^);
          Data3.Text := IntToStr(PInteger(N.Data[3])^);
          Data4.Text := IntToStr(PInteger(N.Data[4])^);
          Data5.Text := IntToStr(PInteger(N.Data[5])^);
          Data6.Text := IntToStr(PInteger(N.Data[6])^);
          Next0.Text := IntToStr(N.NextIndex[0]);
          Next1.Text := IntToStr(N.NextIndex[1]);
          Next2.Text := IntToStr(N.NextIndex[2]);
          Next3.Text := IntToStr(N.NextIndex[3]);
          Next4.Text := IntToStr(N.NextIndex[4]);
          Next5.Text := IntToStr(N.NextIndex[5]);
          Next6.Text := IntToStr(N.NextIndex[6]);
          Count.Text := IntToStr(N.Count);
          if N <> B.Root then
            N.Destroy;
        end;
    end;
end;

end.

