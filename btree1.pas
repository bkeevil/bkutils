unit btree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  Order = 6;          { or whatever: number of kids }
  Max = Order;    { number of records }
  Min = Order div 2;  { Minimum number of records }

type
  TBTreeData = record
    Key: Pointer;
    Data: Pointer;
  end;
  PBTreeNode = ^TBTreeNode;
  TBTreeNode = record
    Count: Integer;
    Data: array[1..Max] of TBTreeData;
    Next: array[0..Max] of PBTreeNode;
  end;
  TIteratorProc = procedure (Key, Data: Pointer) of object;
  TCompareFunc = function (Key1, Key2: Pointer): Integer;

type
  { TBTree }
  EBTree = class(Exception);
  TBTree = class(TObject)
    private
      FNodeCount: Integer;
      FRecordCount: Integer;
      _Compare: TCompareFunc;
      procedure NodeSearch(Target: Pointer; Node: PBTreeNode; var Found: Boolean; var Location: Integer);
      procedure InsertHere(var Root: PBTreeNode; var Entry: TBTreeData; Location: Integer; var Fixup: PBTreeNode);
      procedure _Insert(var Root: PBTreeNode; var Entry: TBTreeData; var Found: Boolean; var Fixup: PBTreeNode);
      procedure Adjust(var Root: PBTreeNode; Location: Integer);
      procedure DeleteHere(var Root: PBTreeNode; Location: Integer);
      procedure _Remove(var Root: PBTreeNode; Key: Pointer; var Found: Boolean);
    protected
      procedure ClearData(var Data: TBTreeData); virtual;
      function CreateNode: PBTreeNode;
      procedure DestroyNode(var Node: PBTreeNode);
      procedure B_Search(Target: Pointer; P: PBTreeNode; var Found: Boolean; var Node: PBTreeNode; var Location: Integer);
      procedure B_Insert(var Root: PBTreeNode; Entry: TBTreeData; var Found: Boolean);
      procedure B_Remove(var Root: PBTreeNode; Key: Pointer; var Found: Boolean);
      function B_Count(Root: PBTreeNode): Integer;
      procedure B_Iterate(Root: PBTreeNode; proc: TIteratorProc);
      procedure B_Clear(Root: PBTreeNode);
    public
      FRoot: PBTreeNode;
      constructor Create(Compare: TCompareFunc);
      destructor Destroy; override;
      function Search(Key: Pointer; var Data: Pointer): Boolean; overload;
      function Search(Key: NativeInt; var Data: Pointer): Boolean; overload;
      function Insert(Key,Data: Pointer): Boolean; overload;
      function Insert(Key: NativeInt; Data: Pointer): Boolean; overload;
      function Remove(Key: Pointer): Boolean; overload;
      function Remove(Key: NativeInt): Boolean; overload;
      procedure Iterate(proc: TIteratorProc);
      procedure Clear;
      function CalcRecordCount: Integer;
      property RecordCount: Integer read FRecordCount;
      property NodeCount: Integer read FNodeCount;
  end;

function IntegerCompareFunc(Key1, Key2: Pointer): Integer;
function PointerCompareFunc(Key1, Key2: Pointer): Integer;

implementation

// Overflow save integer compare
function IntegerCompareFunc(Key1, Key2: Pointer): Integer;
begin
  if PInteger(Key1) > PInteger(Key2) then
    Result := 1
  else
    if PInteger(Key1) < PInteger(Key2) then
      Result := -1
    else
      Result := 0;
end;

// Overflow save pointer compare that works with 32 or 64 bit systems
function PointerCompareFunc(Key1, Key2: Pointer): Integer;
begin
  if PtrUInt(Key1) > PtrUInt(Key2) then
    Result := 1
  else
    if PtrUInt(Key1) < PtrUInt(Key2) then
      Result := -1
    else
      Result := 0;
end;

constructor TBTree.Create(Compare: TCompareFunc);
begin
  inherited Create;
  _Compare := Compare;
end;

destructor TBTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TBTree.CreateNode: PBTreeNode;
begin
  GetMem(Result,SizeOf(TBTreeNode));
  Assert(Result <> nil,'Could not create BTree node');
  FillChar(Result^,SizeOf(TBTreeNode),#0);
  inc(FNodeCount);
end;

procedure TBTree.DestroyNode(var Node: PBTreeNode);
begin
  FreeMem(Node,SizeOf(TBTreeNode));
  Node := nil;
  dec(FNodeCount);
  Assert(FNodeCount >= 0,'BTree node count less than zero');
end;

function TBTree.Search(Key: Pointer; var Data: Pointer): Boolean;
var
  Node: PBTreeNode;
  Location: Integer;
begin
  B_Search(Key,FRoot,Result,Node,Location);
  if Result then
    begin
      Assert(Assigned(Node),'Found a BTree node but node was nil');
      Data := Node^.Data[Location].Data;
    end;
end;

function TBTree.Search(Key: NativeInt; var Data: Pointer): Boolean;
begin
  Result := Search(PPtrInt(Key),Data);
end;

function TBTree.Insert(Key, Data: Pointer): Boolean;
var
  LData: TBTreeData;
begin
  LData.Key := Key;
  LData.Data := Data;
  B_Insert(FRoot,LData,Result);
  Result := not Result;
end;

function TBTree.Insert(Key: NativeInt; Data: Pointer): Boolean;
begin
  Result := Insert(PPtrInt(Key),Data);
end;

function TBTree.Remove(Key: Pointer): Boolean;
begin
  B_Remove(FRoot,Key,Result);
end;

function TBTree.Remove(Key: NativeInt): Boolean;
begin
  Result := Remove(PPtrInt(Key));
end;

procedure TBTree.Iterate(Proc: TIteratorProc);
begin
  B_Iterate(FRoot,Proc);
end;

procedure TBTree.Clear;
begin
  B_Clear(FRoot);
  FRoot := nil;
  Assert(FNodeCount = 0,'NodeCount should be zero');
  Assert(FRecordCount = 0,'RecordCount should be zero');
end;

function TBTree.CalcRecordCount: Integer;
begin
  Result := B_Count(FRoot);
end;

function TBTree.B_Count(Root: PBTreeNode): Integer;
var
  I: Integer;
  N: PBTreeNode;
begin
  if Root = nil then
    begin
      Result := 0;
      Exit;
    end
  else
    begin
      Result := Root^.Count;
      for I := 0 to Result do
        begin
          N := Root^.Next[I];
          if N <> nil then
            Result := Result + B_Count(N);
        end;
    end;
end;

procedure TBTree.B_Iterate(Root: PBTreeNode; Proc: TIteratorProc);
var
  I: Integer;
  N: PBTreeNode;
  D: TBTreeData;
begin
  if Root = nil then Exit;
  for I := 1 to Root^.Count do
    begin
      N := Root^.Next[I-1];
      if N <> nil then
        B_Iterate(N,Proc);
      D := Root^.Data[I];
      Proc(D.Key,D.Data);
    end;
  N := Root^.Next[Root^.Count];
  if N <> nil then
    B_Iterate(N,Proc);
end;

procedure TBTree.B_Clear(Root: PBTreeNode);
var
  I: Integer;
  N: PBTreeNode;
begin
  if Root = nil then Exit;
  for I := 0 to Root^.Count do
    begin
      N := Root^.Next[I];
      if N <> nil then
        B_Clear(N);
    end;
  FRecordCount := FRecordCount - Root^.Count;
  DestroyNode(Root);
end;

procedure TBTree.NodeSearch(Target: Pointer; Node: PBTreeNode; var Found: Boolean; var Location: Integer);
{Searches node for target}
var
  First, Mid, Last: Integer;
  CompareResult: Integer;
begin
  Found := False;
  Location := -1;
  with Node^ do
    if Count > 0 then   { Don't search empty list }
      begin
        First := 1;
        Last := Count;
        while Last > First do   { Usual binary search }
          begin
            Mid := (Last + First) div 2;
            CompareResult := _Compare(Target,Data[Mid].Key);
            if CompareResult > 0 then
              First := Mid + 1
            else
              Last := Mid;
          end;
        Location := Last;
        CompareResult := _Compare(Target,Data[Last].Key);
        if CompareResult < 0 then
          Location := Last - 1  {Adjust subscript }
        else
          if CompareResult = 0 then
            Found := True;     {Indicate Success }
      end;
end;

procedure TBTree.B_Search(Target: Pointer; P: PBTreeNode; var found: Boolean; var Node: PBTreeNode; var Location: Integer);
{Searches B-Tree for target, reports success or failure in Found; returns node and Location if found }
begin
  if P = nil then
    Found := False
  else
    begin
      NodeSearch(Target,P,Found,Location);   { try current node }
      if Found then
        Node := P  { return root as node }
      else         { make recursive call }
        B_Search(Target,P^.Next[Location],Found,Node,Location);
    end;
end;

procedure TBTree.InsertHere(var Root: PBTreeNode; var Entry: TBTreeData; Location: Integer; var Fixup: PBTreeNode);
{Inserts record in this node, splits node if it overflows.  If split occured, returns with Fixup pointing to new node and with entry holding record to be moved up }
var
  I,J,Middle: Integer;
  TempEntry: TBTreeData; { overflow }
  TempPtr: PBTreeNode;      { area }
begin
  with Root^ do
    begin
      if Location > Count+1 then  { Allow caller to append without knowing count }
        Location := Count + 1;
      TempEntry := Data[Max];   { in case of overflow }
      TempPtr := Next[Max];
      for I := Max downto Location + 1 do
        begin
          Data[I] := Data[I-1];
          Next[I] := Next[I-1];
        end;
      if Location <= Max then
        begin                  { insert in node }
          Data[Location] := Entry;
          Next[Location] := Fixup;
        end
      else
        begin                  { Save for split }
          TempEntry := Entry;
          TempPtr := Fixup;
        end;
      Count := Count + 1;
      if Count <= Max then
        Fixup := nil               { overflow? }
      else
        begin
          Middle := Max div 2 + 1;    { --yes: split node }
          Count := Middle - 1;
          Fixup := CreateNode;
          Fixup^.Next[0] := Next[Middle];
          J := 1;
          for I := Middle + 1 to Max do
            begin
              Fixup^.Data[J] := Data[I];     { copy data & pointers into new node }
              Fixup^.Next[J] := Next[I];
              Next[I] := nil;
              ClearData(Data[I]);
              J := J + 1;
            end;
          Next[Middle] := nil;
          Fixup^.Data[J] := TempEntry;
          Fixup^.Next[J] := TempPtr;
          Fixup^.Count := J;
          //for I := J + 1 to Max do      {Not needed because CreateNode clears memory}
          //  Fixup^.Next[I] := nil;
          Entry := Data[Middle];              { data to be moved up }
          ClearData(Data[Middle]);
        end;
    end;
end;

procedure TBTree._Insert(var Root: PBTreeNode; var Entry: TBTreeData; var Found: Boolean; var Fixup: PBTreeNode);
var  { procedure for entering new item in a node }
  Location: Integer;
begin
  NodeSearch(Entry.Key,Root,Found,Location); { look for it here }
  if not Found then  { if found we're in trouble }
    with Root^ do
      begin
        if Next[Location] = nil then    { at a leaf? }
          InsertHere(Root,Entry,Location+1,Fixup)
        else
          begin  { no: recurse }
            _Insert(Next[Location],Entry,Found,Fixup);
            if Fixup <> nil then      { Insertion split node?}
              InsertHere(Root,Entry,Location+1,Fixup);   { -- yes: insert record }
          end;
      end;
end;

procedure TBTree.B_Insert(var Root: PBTreeNode; Entry: TBTreeData; var Found: Boolean);
var           { main insertion routine for b-trees }
  P, Fixup: PBTreeNode;
  //I: Integer;
begin
  Found := False;
  Fixup := nil;
  if Root <> nil then
    begin                       { tree already exists: insert record in it }
      _Insert(Root,Entry,Found,Fixup);
      P := Root;
    end;
  if (Root = nil) or (Fixup <> nil) then
    begin                           { either no root yet or must have split }
      P := Root;
      Root := CreateNode;
      with Root^ do
        begin
          Count := 1;
          Data[1] := Entry;
          Next[0] := P;
          Next[1] := Fixup;
          // Not needed because CreateNode zeros memory of new node
          //for I := 2 to Max do { Init New node...Set remaining pointers to nil because no other kids yet }
          //  Next[I] := nil;
        end;
    end;
  if not Found then inc(FRecordCount);
end;

procedure TBTree.Adjust(var Root: PBTreeNode; Location: Integer);
{ Fixes up nodes with too few records }
var
  T: PBTreeNode;
  Fixup, Q, R: PBTreeNode;
  Rec1, Rec2: TBTreeData;
  i,LeftCount,RightCount: Integer;
begin
  //Main.Memo.Lines.Add(Format('  Adjust(%p,%d)',[Root,Location]));
  with Root^ do
    begin

      if Location = 0 then
        LeftCount := 0                { no left neighbour }
      else
        begin
          T := Next[Location-1];  // Original
          LeftCount := T^.Count;
        end;

      //if Location = Count + 1 then
      if Location = Count then             { no right neighbour }
        RightCount := 0
      else
        begin
          T := Next[Location+1];  // Original
          RightCount := T^.Count;
        end;


      if LeftCount > Min then         { rotate right }
        begin
          Fixup := Next[Location]^.Next[0];
          Q := Next[Location - 1]^.Next[LeftCount];
          Rec1 := Next[Location-1]^.Data[LeftCount];   { Record coming up }
          Rec2 := Data[Location];                   { Record going down }
          InsertHere(Next[Location],Rec2,1,Fixup);
          DeleteHere(Next[Location-1],MaxInt);
          Next[Location]^.Next[0] := Q;
          Data[Location] := Rec1;
        end
      else
        if RightCount > Min then    { Rotate left }
          begin
            Fixup := Next[Location+1]^.Next[0];
            Rec1 := Next[Location+1]^.Data[1];   { Record coming up }
            Rec2 := Data[Location+1];            { Record going down }
            InsertHere(Next[Location],Rec2,MaxInt,Fixup);
            Q := Next[Location+1]^.Next[1];
            DeleteHere(Next[Location+1],1);
            Next[Location+1]^.Next[0] := Q;
            Data[Location+1] := Rec1;
          end
        else
          begin  { Merge }
            if LeftCount > RightCount then
              Location := Location - 1;
            Q := Next[Location];
            R := Next[Location+1];
            Fixup := R^.Next[0];
            InsertHere(Q,Data[Location+1],MaxInt,Fixup);
            for I := 1 to R^.Count do
              InsertHere(Q,R^.Data[i],MaxInt,R^.Next[I]);
            DestroyNode(R);
            R := nil;
            DeleteHere(Root,Location+1);
          end;
    end;
end;

procedure TBTree.DeleteHere(var Root: PBTreeNode; Location: Integer);
{ Removes record at Location and adjusts count }
var
  I: Integer;
begin
  with Root^ do
    begin
      if Location > Count then {Allows caller to access end without knowing count }
        Location := Count;
      for I := Location to Max - 1 do
        begin
          Data[I] := Data[I+1];
          Next[I] := Next[I+1];
        end;
      ClearData(Data[Max]);
      Next[Max] := nil;
      Count := Count - 1;
    end;
end;

procedure TBTree._Remove(var Root: PBTreeNode; Key: Pointer; var Found: Boolean);
{ Recursive B-tree deletion procedure: finds node and calls delete_here for final removal }
var
  P: PBTreeNode;
  Location: Integer;
begin
  //Main.Memo.Lines.Add(Format('  _Remove(%p,%d)',[Root,Key]));
  NodeSearch(Key,Root,Found,Location);   { look for it here }
  with Root^ do
    begin
      if Found then
        if Next[Location - 1] = nil then   { if leaf, delete }
          DeleteHere(Root,Location)
        else
          begin
            p := Next[Location];
            while P^.Next[0] <> nil do   { else find successor }
              P := P^.Next[0];
            Data[Location] := P^.Data[1];     { & copy }
            _Remove(Next[Location],Data[Location].Key,Found);
          end
      else       { not Found }        { Recursive Call }
        if Next[Location] <> nil then
          _Remove(Next[Location],Key,Found);
      {Fixup if necessary }
      if (Next[Location] <> nil) and (Next[Location]^.Count < Min) then
        Adjust(Root,Location);
    end;
end;

procedure TBTree.ClearData(var Data: TBTreeData);
begin
  Data.Key := nil;
  Data.Data := nil;
end;

procedure TBTree.B_Remove(var Root: PBTreeNode; Key: Pointer; var Found: Boolean);
{main procedure for deletion from a B-Tree }
var
  P: PBTreeNode;
begin
  //Main.Memo.Lines.Add('B_Remove('+IntToStr(Key)+')');
  Found := False;
  if Root <> nil then        { Don't delete from an empty tree }
    begin
      _Remove(Root,Key,Found);
      if Root^.Count = 0 then   { root empty?}
        begin
          //Main.Memo.Lines.Add('  Root is empty, using next level down');
          P := Root;   { yes: Next level down is root }
          Root := Root^.Next[0];
          DestroyNode(P);
        end;
    end;
  if Found then
    begin
      dec(FRecordCount);
      Assert(FRecordCount >= 0,'BTree record count less than zero');
    end;
end;

end.

