unit btreefile;

{$mode objfpc}{$H+}

{$define DEBUG}
{.$define testdriver}
interface

uses
  Classes, SysUtils, Files, Logging;

type
  TNodeIndex = Cardinal;
  PNodeIndex = ^TNodeIndex;

  TBTreeFileHandle = class;

  { TBTreeNode }

  TBTreeNode = class(TLogObject)
    private
      function GetCount: Word;
      function GetNextIndex(Index: Word): TNodeIndex;
      procedure SetCount(AValue: Word);
      function GetData(Index: Word): Pointer;
      function GetNext(Index: Word): TBTreeNode;
      procedure SetNext(Index: Word; AValue: TBTreeNode);
      procedure SetNextIndex(Index: Word; AValue: TNodeIndex);
    protected
      FTree: TBTreeFileHandle;
      FPage: PBufferedPage;
      procedure Validate;
    public
      constructor Create(ATree: TBTreeFileHandle);
      constructor Create(ATree: TBTreeFileHandle; PageIdx: TPageIndex);
      destructor Destroy; override;
      property Tree: TBTreeFileHandle read FTree;
      property Page: PBufferedPage read FPage;
      property Count: Word read GetCount write SetCount;
      property Data[Index: Word]: Pointer read GetData;
      property Next[Index: Word]: TBTreeNode read GetNext write SetNext;
      property NextIndex[Index: Word]: TNodeIndex read GetNextIndex write SetNextIndex;
      {$IFDEF testdriver}
      function AsString: String;
      {$ENDIF}
  end;

  TIteratorProc = procedure (Rec: Pointer) of object;
  TCompareFunc = function (Rec1, Rec2: Pointer): Integer;

  { TBTree }
  EBTree = class(Exception);

  { TBTreeFileHandle }

  TBTreeFileHandle = class(TCustomFileHandle)
    private
      FRoot       : TBTreeNode;
      FNodeCount  : Cardinal;  // Number of allocated nodes.  Not Equal to RecordCount
      FOrder      : Word;
      FStream     : TStream;
      _Compare    : TCompareFunc;
      function GetRecordCount: Cardinal;
      function GetRecordSize: Word;
      procedure RemoveTrailingZeros;
      procedure SetRecordCount(AValue: Cardinal);
      procedure SetRootIndex;
      procedure NullIteratorProc(Rec: Pointer);
      procedure SaveToStreamProc(Rec: Pointer);
      procedure NodeSearch(Rec: Pointer; Node: TBTreeNode; var Found: Boolean; var Location: Integer);
      procedure InsertHere(Root: TBTreeNode; Rec: Pointer; Location: Integer; var Fixup: TBTreeNode);
      procedure SetRecordSize(AValue: Word);
      procedure _Insert(var Root: TBTreeNode; Rec: Pointer; var Found: Boolean; var Fixup: TBTreeNode);
      procedure GetChildCounts(Root: TBTreeNode; Location: Integer; var LeftCount, RightCount: Integer);
      procedure Merge(Node: TBTreeNode; Location, LeftCount, RightCount: Integer);
      procedure RotateLeft(Node: TBTreeNode; Location: Integer);
      procedure RotateRight(Node: TBTreeNode; Location: Integer; LeftCount: Integer);
      procedure Adjust(var Root: TBTreeNode; Location: Integer);
      procedure DeleteHere(Root: TBTreeNode; Location: Integer);
      procedure _Remove(Root: TBTreeNode; Rec: Pointer; var Found: Boolean);
      //
      procedure Truncate(PageIdx: TPageIndex);
      procedure DeletePage(var Page: PBufferedPage);
      function AllocPage: PBufferedPage;
      function FetchPage(PageIdx: TPageIndex): PBufferedPage;
      procedure ReleasePage(var Page: PBufferedPage);
    protected
      class function CalculateOrder(RecordSize: Word): Word;
      class function CalculatePageSize(RecordSize: Word; Order: Word): Word;
      procedure RootChanged;
      procedure B_Search(Rec: Pointer; P: TBTreeNode; var Found: Boolean; var Node: TBTreeNode; var Location: Integer);
      procedure B_Insert(var Root: TBTreeNode; Rec: Pointer; var Found: Boolean);
      procedure B_Remove(var Root: TBTreeNode; Rec: Pointer; var Found: Boolean);
      procedure B_Iterate(Root: TBTreeNode; proc: TIteratorProc; var RecordCount: Cardinal);
    public // Normally protected but make public for testing
      function CalcRecordCount: Integer;
      //
      property Root: TBTreeNode read FRoot;
      property NodeCount: Cardinal read FNodeCount;
      property Order: Word read FOrder write FOrder;
    public
      constructor Create(AFile: TFile); override;
      destructor Destroy; override;
      procedure Init(ARecordSize: Word; Compare: TCompareFunc);
      //
      function Search(Rec: Pointer): Boolean;
      function Insert(Rec: Pointer): Boolean;
      function Update(Rec: Pointer): Boolean;
      function Remove(Rec: Pointer): Boolean;
      procedure Iterate(Proc: TIteratorProc);
      procedure Clear;
      procedure LoadFromStream(Stream: TStream);
      procedure SaveToStream(Stream: TStream);
      procedure Flush; override;
      //
      property RecordCount: Cardinal read GetRecordCount;
      property RecordSize: Word read GetRecordSize write SetRecordSize;
  end;

function IntegerCompareFunc(Rec1, Rec2: Pointer): Integer;

implementation

{$IFDEF testdriver}
uses
  btreetesthelper;
{$ENDIF}

type
  TFileHack = class(TFile);
  TFileFATHack = class(TFileFAT);

{$IFDEF testdriver}
function RecToStr(Rec: Pointer): String;
begin
  Result := Format('{%d,%d}',[PRec(Rec)^.Key,PRec(Rec)^.Data]);
end;
{$ENDIF}

// Overflow save integer compare
function IntegerCompareFunc(Rec1, Rec2: Pointer): Integer;
begin
  if PInteger(Rec1)^ > PInteger(Rec2)^ then
    Result := 1
  else
    if PInteger(Rec1)^ < PInteger(Rec2)^ then
      Result := -1
    else
      Result := 0;
end;

{ TBTreeNode }

constructor TBTreeNode.Create(ATree: TBTreeFileHandle);
begin
  {$IFNDEF DEBUG}Log.Filter := [mtInfo,mtError];{$ENDIF}
  FTree := ATree;
  inc(FTree.FNodeCount);
  FPage := FTree.AllocPage;
end;

constructor TBTreeNode.Create(ATree: TBTreeFileHandle; PageIdx: TPageIndex);
begin
  {$IFNDEF DEBUG}Log.Filter := [mtInfo,mtError];{$ENDIF}
  FTree := ATree;
  inc(FTree.FNodeCount);
  FPage := FTree.FetchPage(PageIdx);
end;

destructor TBTreeNode.Destroy;
begin
  dec(FTree.FNodeCount);
  if FPage <> nil then
    FTree.ReleasePage(FPage);
  inherited Destroy;
end;

function TBTreeNode.GetCount: Word;
begin
  Result := PWord(FPage^.Data)^;
end;

procedure TBTreeNode.SetCount(AValue: Word);
begin
  PWord(FPage^.Data)^ := AValue;
end;

function TBTreeNode.GetData(Index: Word): Pointer;
var
  Offset: PtrInt;
begin
  Offset := SizeOf(Word) + ((Index - 1) * FTree.RecordSize);
  Result := Pointer(FPage^.Data + Offset);
end;

function TBTreeNode.GetNextIndex(Index: Word): TNodeIndex;
var
  Offset: PtrInt;
begin
  Assert(Assigned(FTree),'Tree not assigned');
  Offset := SizeOf(Word) + (FTree.FOrder * FTree.RecordSize) + (Index * SizeOf(TPageIndex));
  Result := PNodeIndex(FPage^.Data+Offset)^;
end;

function TBTreeNode.GetNext(Index: Word): TBTreeNode;
var
  NodeIndex: TNodeIndex;
begin
  NodeIndex := GetNextIndex(Index);
  if NodeIndex = 0 then
    Result := nil
  else
    Result := TBTreeNode.Create(FTree,NodeIndex-1);
end;

procedure TBTreeNode.SetNextIndex(Index: Word; AValue: TNodeIndex);
var
  Offset: PtrInt;
begin
  Offset := SizeOf(Word) + (FTree.FOrder * FTree.RecordSize) + (Index * SizeOf(TPageIndex));
  PNodeIndex(FPage^.Data+Offset)^ := AValue;
end;

procedure TBTreeNode.SetNext(Index: Word; AValue: TBTreeNode);
begin
  if Assigned(AValue) then
    begin
      Assert(Assigned(AValue.FPage));
      SetNextIndex(Index,AValue.FPage^.PageIdx+1);
    end
  else
    SetNextIndex(Index,0);
end;

{$IFDEF testdriver}
function TBTreeNode.AsString: String;
var
  X: Integer;
begin
  if not Assigned(FPage) then
    begin
      Result := '';
      Exit;
    end;
  Result := IntToStr(FPage^.PageIdx+1)+'{'+IntToStr(Count)+',{';
  for X := 1 to FTree.FOrder do
    begin
      Result := Result + '['+IntToStr(X)+']'+IntToStr(PRec(Data[X])^.Key)+':'+IntToStr(PRec(Data[X])^.Data);
      if X < FTree.FOrder then
        Result := Result + ',';
    end;
  Result := Result+'},{';
  for X := 0 to FTree.FOrder do
    begin
      Result := Result + '['+IntToStr(X)+']'+IntToStr(NextIndex[X]);
      if X < FTree.FOrder then
        Result := Result + ',';
    end;
  Result := Result + '}}';
end;
{$ENDIF}

procedure TBTreeNode.Validate;
var
  I: Integer;
begin
  for I := Tree.Order downto Count + 1 do
    if NextIndex[I] <> 0 then
      raise EBTree.Create('BTreeNode validation failed');
end;

{ TBTreeFileHandle }

constructor TBTreeFileHandle.Create(AFile: TFile);
begin
  inherited Create(AFile);
  TFileHack(_File).Buffer.Mode := bmRandomAccess;
end;

procedure TBTreeFileHandle.Init(ARecordSize: Word; Compare: TCompareFunc);
var
  RootNodeIndex: Cardinal;
  {$IFDEF DEBUG}
  PageSize: Word;
  {$ENDIF}
begin
  _Compare := Compare;
  RecordSize := ARecordSize;
  FOrder := CalculateOrder(RecordSize);
  {$IFDEF DEBUG}
  PageSize := CalculatePageSize(RecordSize,FOrder);
  Log.Send(mtDebug,'Init BTree %s with RecordSize=%d, Order=%d, PageSize=%d',[_File.Name,RecordSize,FOrder,PageSize]);
  {$ENDIF}
  if Order < 6 then
    raise EBTree.Create('BTree order must be at least 6');
  RootNodeIndex := _File.RootNode;
  if RootNodeIndex > 0 then
    FRoot := TBTreeNode.Create(Self,RootNodeIndex - 1);
end;

destructor TBTreeFileHandle.Destroy;
begin
  if Assigned(FRoot) then
    FRoot.Destroy;
  Flush;
  inherited Destroy;
end;

class function TBTreeFileHandle.CalculateOrder(RecordSize: Word): Word;
begin
  Result := (PAGE_SIZE - SizeOf(Word) - SizeOf(TPageNum)) div (RecordSize + SizeOf(TPageNum));
end;

class function TBTreeFileHandle.CalculatePageSize(RecordSize: Word; Order: Word): Word;
begin
  Result := SizeOf(Word) + SizeOf(TPageNum) + ((SizeOf(TPageNum) + RecordSize) * Order);
end;

function TBTreeFileHandle.GetRecordSize: Word;
begin
  Result := _File.RecordSize;
end;

procedure TBTreeFileHandle.SetRecordSize(AValue: Word);
begin
  _File.RecordSize := AValue;
end;

function TBTreeFileHandle.GetRecordCount: Cardinal;
begin
  Result := _File.RecordCount;
end;

procedure TBTreeFileHandle.SetRecordCount(AValue: Cardinal);
begin
  _File.RecordCount := AValue;
end;

function TBTreeFileHandle.AllocPage: PBufferedPage;
begin
  Result := TFileHack(_File).Alloc;
end;

function TBTreeFileHandle.FetchPage(PageIdx: TPageIndex): PBufferedPage;
begin
  Result := TFileHack(_File).Fetch(PageIdx);
end;

procedure TBTreeFileHandle.ReleasePage(var Page: PBufferedPage);
begin
  TFileHack(_File).Release(Page);
end;

procedure TBTreeFileHandle.DeletePage(var Page: PBufferedPage);
begin
  if Page <> nil then
    begin
      Page^.Modified := False;
      TFileHack(_File).Delete(Page^.PageIdx);
      Page := nil;
    end;
end;

procedure TBTreeFileHandle.Truncate(PageIdx: TPageIndex);
begin
  TFileHack(_File).Truncate(PageIdx);
end;

procedure TBTreeFileHandle.SetRootIndex;
begin
  if Assigned(FRoot) then
    _File.RootNode := FRoot.FPage^.PageIdx + 1
  else
    _File.RootNode := 0;
end;

procedure TBTreeFileHandle.RootChanged;
begin
  if Assigned(FRoot) then
    if FRoot.FPage^.PageIdx + 1 = _File.RootNode then
      Exit
    else
      FRoot.Destroy;
  if _File.RootNode > 0 then
    FRoot := TBTreeNode.Create(Self,_File.RootNode - 1)
  else
    FRoot := nil;
end;

function TBTreeFileHandle.Search(Rec: Pointer): Boolean;
var
  Node: TBTreeNode = nil;
  Location: Integer;
begin
  if FRoot = nil then
    if PageCount > 0 then
      if _File.RootNode > 0 then
        FRoot := TBTreeNode.Create(Self,_File.RootNode-1)
      else
        raise EBTree.Create('Cannot find root node in btree')
    else
      begin
        Result := False;
        Exit;
      end;

  B_Search(Rec,FRoot,Result,Node,Location);
  if Result then
    begin
      Assert(Assigned(Node),'Found a BTree node but node was nil');
      Move(Node.Data[Location]^,Rec^,RecordSize);
      if Node <> FRoot then
        Node.Destroy;
    end;
  Flush;
end;

function TBTreeFileHandle.Insert(Rec: Pointer): Boolean;
var
  LRoot: TBTreeNode;
begin
  LRoot := FRoot;
  if FRoot = nil then
    if PageCount > 0 then
      if _File.RootNode > 0 then
        FRoot := TBTreeNode.Create(Self,_File.RootNode-1)
      else
        raise EBTree.Create('Cannot find root node in btree');

  B_Insert(FRoot,Rec,Result);
  Result := not Result;
  Assert(Assigned(FRoot) or (PageCount = 0));
  if LRoot <> FRoot then
    SetRootIndex;
  Flush;
end;

function TBTreeFileHandle.Update(Rec: Pointer): Boolean;
var
  Node: TBTreeNode = nil;
  Location: Integer;
begin
  if FRoot = nil then
    if PageCount > 0 then
      if _File.RootNode > 0 then
        FRoot := TBTreeNode.Create(Self,_File.RootNode-1)
      else
        raise EBTree.Create('Cannot find root node in btree')
    else
      begin
        Result := False;
        Exit;
      end;

  B_Search(Rec,FRoot,Result,Node,Location);
  if Result then
    begin
      Assert(Assigned(Node),'Found a BTree node but node was nil');
      Move(Rec^,Node.Data[Location]^,RecordSize);
      if Node <> FRoot then
        Node.Destroy;
    end;
  Flush;
end;

function TBTreeFileHandle.Remove(Rec: Pointer): Boolean;
var
  LRoot: TBTreeNode;
begin
  LRoot := FRoot; // Store previous root node

  if FRoot = nil then
    if PageCount > 0 then
      if _File.RootNode > 0 then
        FRoot := TBTreeNode.Create(Self,_File.RootNode-1)
      else
        raise EBTree.Create('Cannot find root node in btree')
    else
      begin
        Result := False;
        Exit;
      end;

  Flush;
  B_Remove(FRoot,Rec,Result);
  Flush;
  // Determine if root node changed
  if LRoot <> FRoot then
    SetRootIndex;
end;

procedure TBTreeFileHandle.Iterate(Proc: TIteratorProc);
var
  LRecordCount: Cardinal = 0;
begin
  if (FRoot = nil) and (PageCount > 0) then
    FRoot := TBTreeNode.Create(Self,_File.RootNode-1);
  B_Iterate(FRoot,Proc,LRecordCount);
  SetRecordCount(LRecordCount);
  Flush;
end;

procedure TBTreeFileHandle.NullIteratorProc(Rec: Pointer);
begin
  // Intentionally Empty.  Used by CalcRecordCount.
end;

function TBTreeFileHandle.CalcRecordCount: Integer;
begin
  Iterate(@NullIteratorProc);
  Result := RecordCount;
end;

procedure TBTreeFileHandle.Clear;
begin
  if (FRoot <> nil) then
    FRoot.Destroy;
  if (PageCount > 0) then
    Truncate(0);
  Flush;
  SetRecordCount(0);
  FRoot := nil;
  SetRootIndex;
  Assert(FNodeCount = 0,'NodeCount should be zero');
end;

procedure TBTreeFileHandle.LoadFromStream(Stream: TStream);
var
  LCount: Cardinal;
  X: Integer;
  Rec: Pointer;
begin
  LCount := Stream.Size div RecordSize;
  if Stream.Size mod RecordSize > 0 then
    raise EBTree.Create('Stream size is not an exact multiple of record size');
  Clear;
  Stream.Position := 0;
  GetMem(Rec,RecordSize);
  try
    for X := 1 to LCount do
      begin
        Stream.Read(Rec^,RecordSize);
        Insert(Rec);
      end;
  finally
    FreeMem(Rec,RecordSize);
  end;
end;

procedure TBTreeFileHandle.SaveToStreamProc(Rec: Pointer);
begin
  FStream.Write(Rec^,RecordSize);
end;

procedure TBTreeFileHandle.SaveToStream(Stream: TStream);
begin
  FStream := Stream;
  FStream.Size := 0;
  Iterate(@SaveToStreamProc);
end;

// Removes entries from the end of the file's FAT table that have been set to zero
procedure TBTreeFileHandle.RemoveTrailingZeros;
begin
  TFileFATHack(TFileHack(_File).FAT).RemoveTrailingZeros;
end;

procedure TBTreeFileHandle.Flush;
begin
  inherited Flush;
  TFileFATHack(TFileHack(_File).FAT).RemoveTrailingZeros;
end;

// *** BTREE Routines

procedure TBTreeFileHandle.B_Iterate(Root: TBTreeNode; Proc: TIteratorProc; var RecordCount: Cardinal);
var
  I: Integer;
  N: TBTreeNode;
begin
  if Root = nil then Exit;
  RecordCount := RecordCount + Root.Count;
  for I := 1 to Root.Count do
    begin
      N := Root.Next[I-1];
      if N <> nil then
        begin
          B_Iterate(N,Proc,RecordCount);
          N.Destroy;
        end;
      Proc(Root.Data[I]);
    end;
  N := Root.Next[Root.Count];
  if N <> nil then
    begin
      B_Iterate(N,Proc,RecordCount);
      N.Destroy;
    end;
end;

procedure TBTreeFileHandle.NodeSearch(Rec: Pointer; Node: TBTreeNode; var Found: Boolean; var Location: Integer);
{Searches node for target}
var
  First, Mid, Last: Integer;
  CompareResult: Integer;
begin
  Found := False;
  Location := -1;
  with Node do
    if Count > 0 then   { Don't search empty list }
      begin
        First := 1;
        Last := Count;
        while Last > First do   { Usual binary search }
          begin
            Mid := (Last + First) div 2;
            CompareResult := _Compare(Rec,Data[Mid]);
            if CompareResult > 0 then
              First := Mid + 1
            else
              Last := Mid;
          end;
        Location := Last;
        CompareResult := _Compare(Rec,Data[Last]);
        if CompareResult < 0 then
          Location := Last - 1  {Adjust subscript }
        else
          if CompareResult = 0 then
            Found := True;     {Indicate Success }
      end;
end;

procedure TBTreeFileHandle.B_Search(Rec: Pointer; P: TBTreeNode; var Found: Boolean; var Node: TBTreeNode; var Location: Integer);
{Searches B-Tree for target, reports success or failure in Found; returns node and Location if found }
var
  N: TBTreeNode;
begin
  if P = nil then
    Found := False
  else
    begin
      NodeSearch(Rec,P,Found,Location);   { try current node }
      if Found then
        Node := P  { return root as node }
      else         { make recursive call }
        begin
          N := P.Next[Location];
          B_Search(Rec,N,Found,Node,Location);
          if N <> Node then
            N.Destroy;
        end;
    end;
end;

// Fixup is inserted as the right next node of Rec
procedure TBTreeFileHandle.InsertHere(Root: TBTreeNode; Rec: Pointer; Location: Integer; var Fixup: TBTreeNode);
{Inserts record in this node, splits node if it overflows.  If split occured, returns with Fixup pointing to new node and with entry holding record to be moved up }
var
  I,J,Middle: Integer;
  TempRec: Pointer;         { overflow }
  TempIdx: TNodeIndex;
begin
  GetMem(TempRec,RecordSize);
  with Root do
    begin
      FPage^.Modified := True;
      if Location > Count+1 then  { Allow caller to append without knowing count }
        Location := Count + 1;
      Move(Data[FOrder]^,TempRec^,RecordSize);                  { Save the last element of the Data array in TempRec in case of insert overlow }
      TempIdx := NextIndex[FOrder]; //TempPtr := Next[FOrder];  { Save the last element of the Next array in TempPtr in case of insert overflow }

      for I := FOrder downto Location + 1 do                    { Perform the insert }
        begin
          Move(Data[I-1]^,Data[I]^,RecordSize);
          NextIndex[I] := NextIndex[I-1]; //Next[I] := Next[I-1];
        end;

      if Location <= FOrder then
        begin                  { insert in node }
          Move(Rec^,Data[Location]^,RecordSize);
          Next[Location] := Fixup; //Next[Location] := Fixup;
        end
      else
        begin                  { Save for split }
          Move(Rec^,TempRec^,RecordSize);
          if Fixup <> nil then
            begin
              TempIdx := Fixup.FPage^.PageIdx + 1;
              Fixup.FPage^.Modified := True;
            end
          else
            TempIdx := 0;
        end;
      Count := Count + 1;
      if Count <= FOrder then
        begin
          if Assigned(Fixup) then
            Fixup.Destroy;
          Fixup := nil;               { overflow? }
        end
      else
        begin
          Middle := FOrder div 2 + 1;    { --yes: split node }
          Count := Middle - 1;
          if Assigned(Fixup) then                      { Fixup will contain everything after middle }
            Fixup.Destroy;
          Fixup := TBTreeNode.Create(Self); // Create a new node
          Fixup.NextIndex[0] := NextIndex[Middle]; //Fixup.Next[0] := Next[Middle];
          J := 1;
          for I := Middle + 1 to FOrder do
            begin
              Move(Data[I]^,Fixup.Data[J]^,RecordSize);
              Fixup.NextIndex[J] := NextIndex[I];  //Fixup.Next[J] := Next[I];
              NextIndex[I] := 0; //Next[I] := nil;
              FillChar(Data[I]^,RecordSize,0);
              J := J + 1;
            end;
          NextIndex[Middle] := 0; //Next[Middle] := nil;
          Move(TempRec^,Fixup.Data[J]^,RecordSize);
          Fixup.NextIndex[J] := TempIdx; //Fixup.Next[J] := TempPtr;
          Fixup.Count := J;
          Move(Data[Middle]^,Rec^,RecordSize);
          FillChar(Data[Middle]^,RecordSize,0);
          FPage^.Modified := True;
        end;
    end;
  FreeMem(TempRec,RecordSize);
end;

procedure TBTreeFileHandle._Insert(var Root: TBTreeNode; Rec: Pointer; var Found: Boolean; var Fixup: TBTreeNode);
var  { procedure for entering new item in a node }
  Location: Integer;
  Node: TBTreeNode;
begin
  NodeSearch(Rec,Root,Found,Location); { look for it here }
  if not Found then  { if found we're in trouble }
    with Root do
      begin
        if NextIndex[Location] = 0 then // if Next[Location] = nil then    { at a leaf? }
          InsertHere(Root,Rec,Location+1,Fixup)
        else
          begin  { no: recurse }
            Node := Next[Location];
            _Insert(Node,Rec,Found,Fixup);
            Node.Destroy;
            if Fixup <> nil then      { Insertion split node?}
              begin
                InsertHere(Root,Rec,Location+1,Fixup);   { -- yes: insert record }
              end;
          end;
      end;
end;

procedure TBTreeFileHandle.B_Insert(var Root: TBTreeNode; Rec: Pointer; var Found: Boolean);
var           { main insertion routine for b-trees }
  P, Fixup: TBTreeNode;
begin
  Found := False;
  Fixup := nil;
  if Root <> nil then
    begin                       { tree already exists: insert record in it }
      _Insert(Root,Rec,Found,Fixup);
      P := Root;
    end;
  if (Root = nil) or (Fixup <> nil) then
    begin                           { either no root yet or must have split }
      P := Root;
      Root := TBTreeNode.Create(Self);
      Root.Count := 1;
      Move(Rec^,Root.Data[1]^,RecordSize);
      Root.Next[0] := P;
      Root.Next[1] := Fixup;
      Root.FPage^.Modified := True;
      if Assigned(P) then
        P.Destroy;
      if Assigned(Fixup) then
        Fixup.Destroy;
    end;
  if not Found then SetRecordCount(RecordCount + 1);
end;

procedure TBTreeFileHandle.RotateRight(Node: TBTreeNode; Location: Integer; LeftCount: Integer);
var
  Rec1,Rec2: Pointer;
  Q,R,L,Fixup: TBTreeNode;
begin
  GetMem(Rec1,RecordSize);
  GetMem(Rec2,RecordSize);
  try
    R := Node.Next[Location];
    L := Node.Next[Location - 1];

    Fixup := R.Next[0];
    Q := L.Next[LeftCount];
    Move(L.Data[LeftCount]^,Rec1^,RecordSize);
    Move(Node.Data[Location]^,Rec2^,RecordSize);
    InsertHere(R,Rec2,1,Fixup);
    DeleteHere(L,MaxInt);
    R.Next[0] := Q;
    Move(Rec1^,Node.Data[Location]^,RecordSize);

    R.FPage^.Modified := True;
    L.FPage^.Modified := True;

    if R <> nil then R.Destroy;
    if L <> nil then L.Destroy;
    if Q <> nil then Q.Destroy;
  finally
    FreeMem(Rec1,RecordSize);
    FreeMem(Rec2,RecordSize);
  end;
end;

procedure TBTreeFileHandle.RotateLeft(Node: TBTreeNode; Location: Integer);
var
  Rec1,Rec2: Pointer;
  Q,L,R,Fixup: TBTreeNode;
begin
  GetMem(Rec1,RecordSize);
  GetMem(Rec2,RecordSize);
  try
    R := Node.Next[Location+1];
    L := Node.Next[Location];

    Fixup := R.Next[0];
    Move(R.Data[1]^,Rec1^,RecordSize);
    Move(Node.Data[Location+1]^,Rec2^,RecordSize);
    InsertHere(L,Rec2,MaxInt,Fixup);
    Q := R.Next[1];
    DeleteHere(R,1);
    R.Next[0] := Q;
    Move(Rec1^,Node.Data[Location+1]^,RecordSize);

    R.FPage^.Modified := True;
    L.FPage^.Modified := True;

    if R <> nil then R.Destroy;
    if L <> nil then L.Destroy;
    if Q <> nil then Q.Destroy;
  finally
    FreeMem(Rec1,RecordSize);
    FreeMem(Rec2,RecordSize);
  end;
end;

// Merges the left and right child of Data[Location+1] into the node pointed to by Next[Location]
procedure TBTreeFileHandle.Merge(Node: TBTreeNode; Location, LeftCount, RightCount: Integer);
var
  L,R,T: TBTreeNode;
  I: Integer;
begin
  if LeftCount > RightCount then
    Location := Location - 1;
  // Retrieve the nodes used in the merge
  L := Node.Next[Location];
  R := Node.Next[Location+1];
  // Append the data at Location+1 to the left child node.
  T := R.Next[0];
  InsertHere(L,Node.Data[Location+1],MaxInt,T);
  if T <> nil then T.Destroy;
  DeleteHere(Node,Location+1);
  // Insert the data and pointers
  for I := 1 to R.Count do
    begin
      T := R.Next[I];
      InsertHere(L,R.Data[I],MaxInt,T);
      if T <> nil then
        T.Destroy;
    end;
  // Destroy the right child node and remove the page from the file.
  DeletePage(R.FPage);
  R.Destroy;
  // Ensure that Node and L pages are set to Modified so changes are saved to disk
  L.FPage^.Modified := True;
  Node.FPage^.Modified := True;
  // Left child is no longer needed.  Free it.
  if L <> nil then L.Destroy;
end;

procedure TBTreeFileHandle.GetChildCounts(Root: TBTreeNode; Location: Integer; var LeftCount, RightCount: Integer);
var
  Node: TBTreeNode;
begin
  if Location = 0 then
    LeftCount := 0                { no left neighbour }
  else
    begin
      Node := Root.Next[Location-1];
      LeftCount := Node.Count;
      Node.Destroy;
    end;

  //if Location = Root.Count + 1 then
  if Location = Root.Count then             { no right neighbour }
    RightCount := 0
  else
    begin
      Node := Root.Next[Location+1];
      RightCount := Node.Count;
      Node.Destroy;
    end;
end;

procedure TBTreeFileHandle.Adjust(var Root: TBTreeNode; Location: Integer);
{ Fixes up nodes with too few records }
var
  LeftCount,RightCount: Integer;
  Min: Word;
begin
  Min := FOrder div 2;
  with Root do
    begin
      FPage^.Modified := True;
      GetChildCounts(Root,Location,LeftCount,RightCount);
      if LeftCount > Min then
        RotateRight(Root,Location,LeftCount)
      else
        if RightCount > Min then
          RotateLeft(Root,Location)
        else
          Merge(Root,Location,LeftCount,RightCount);
    end;
end;

procedure TBTreeFileHandle.DeleteHere(Root: TBTreeNode; Location: Integer);
{ Removes record at Location and adjusts count }
var
  I: Integer;
begin
  with Root do
    begin
      FPage^.Modified := True;
      if Location > Count then {Allows caller to access end without knowing count }
        Location := Count;
      for I := Location to FOrder - 1 do
        begin
          Move(Data[I+1]^,Data[I]^,RecordSize);
          NextIndex[I] := NextIndex[I+1];
        end;
      FillChar(Data[FOrder]^,RecordSize,0);
      NextIndex[FOrder] := 0;
      Count := Count - 1;
    end;
end;

procedure TBTreeFileHandle._Remove(Root: TBTreeNode; Rec: Pointer; var Found: Boolean);
{ Recursive B-tree deletion procedure: finds node and calls delete_here for final removal }
var
  P,Q,V: TBTreeNode;
  Location: Integer;
begin
  V := nil;
  V := nil;
  NodeSearch(Rec,Root,Found,Location);   { look for it here }
  with Root do
    begin
      if Found then
        if NextIndex[Location - 1] = 0 then   { if leaf, delete }
          DeleteHere(Root,Location)
        else
          begin
            P := Next[Location];
            while P.NextIndex[0] <> 0 do   { else find successor }
              begin
                Q := P;
                P := P.Next[0];
                if Q <> nil then Q.Destroy;
              end;
            Move(P.Data[1]^,Data[Location]^,RecordSize);
            FPage^.Modified := True;
            V := Next[Location];
            _Remove(V,Data[Location],Found);
            if P <> nil then
              P.Destroy;
          end
      else       { not Found }        { Recursive Call }
        if NextIndex[Location] <> 0 then
          begin
            V := Next[Location];
            _Remove(V,Rec,Found);
          end;
      {Fixup if necessary }
      if V = nil then
        V := Next[Location];
      if V <> nil then
        begin
          if V.Count < (FOrder div 2) then
            Adjust(Root,Location);
          V.Destroy;
        end;
    end;
end;

procedure TBTreeFileHandle.B_Remove(var Root: TBTreeNode; Rec: Pointer; var Found: Boolean);
{main procedure for deletion from a B-Tree }
var
  P: TBTreeNode;
begin
  Found := False;
  if Root <> nil then        { Don't delete from an empty tree }
    begin
      _Remove(Root,Rec,Found);
      if Root.Count = 0 then   { root empty?}
        begin
          P := Root;   { yes: Next level down is root }
          Root := Root.Next[0];
          DeletePage(P.FPage);
          P.Destroy;
        end;
    end;
  if Found then
    begin
      SetRecordCount(RecordCount - 1);
      Assert(RecordCount >= 0,'BTree record count less than zero');
    end;
end;

end.

