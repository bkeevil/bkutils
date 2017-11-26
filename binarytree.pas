unit binarytree;

{$mode objfpc}{$H+}

interface

type
  PTreeNode = ^TTreeNode;
  TTreeNode = record
    Key  : Pointer;
    Data : Pointer;
    Left, Right: PTreeNode;
  end;
  TIteratorProc = procedure (Key, Data: Pointer) of object;
  TCompareFunc = function (Key1, Key2: Pointer): Integer;

  { TBinaryTree }

  TBinaryTree = class(TObject)
    private
      FRoot: PTreeNode;
      FRecordCount: Integer;
      _Compare: TCompareFunc;
      function _Count(Root: PTreeNode): Integer;
      function _Remove(var Root: PTreeNode; Key: Pointer): Boolean;
      function _Insert(var Root: PTreeNode; Node: PTreeNode): Boolean;
      procedure _Iterate(var Root: PTreeNode; Proc: TIteratorProc);
      function _Search(var Root: PTreeNode; Key: Pointer): PTreeNode;
      procedure _Clear(var Root: PTreeNode);
    protected
      function CreateNode(Key: Pointer; Data: Pointer): PTreeNode;
      procedure DestroyNode(var Node: PTreeNode); virtual;
    public
      constructor Create(Compare: TCompareFunc);
      destructor Destroy; override;

      function Insert(Key: Pointer; Data: Pointer): Boolean; overload;
      function Insert(Key: NativeInt; Data: Pointer): Boolean; overload;
      function Search(Key: Pointer; var Data: Pointer): Boolean; overload;
      function Search(Key: NativeInt; var Data: Pointer): Boolean; overload;
      function Remove(Key: Pointer): Boolean; overload;
      function Remove(Key: NativeInt): Boolean; overload;
      procedure Iterate(Proc: TIteratorProc);
      procedure Clear;
      function CalcRecordCount: Integer;
      property RecordCount: Integer read FRecordCount;
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

constructor TBinaryTree.Create(Compare: TCompareFunc);
begin
  inherited Create;
  _Compare := Compare;
end;

destructor TBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TBinaryTree.CreateNode(Key: Pointer; Data: Pointer): PTreeNode;
begin
  Result := New(PTreeNode);
  Result^.Key := Key;
  Result^.Data := Data;
  Result^.Left := nil;
  Result^.Right := nil;
end;

procedure TBinaryTree.DestroyNode(var Node: PTreeNode);
begin
  Dispose(Node);
  Node := nil;
end;

function TBinaryTree.Insert(Key: Pointer; Data: Pointer): Boolean;
begin
  Result := _Insert(FRoot,CreateNode(Key,Data));
  if Result then inc(FRecordCount);
end;

function TBinaryTree.Insert(Key: NativeInt; Data: Pointer): Boolean;
begin
  Result := Insert(PPtrInt(Key),Data);
end;

function TBinaryTree._Insert(var Root: PTreeNode; Node: PTreeNode): Boolean;
var
  CompareResult: Integer;
begin
  Result := True;
  if Root = nil then
    Root := Node
  else
    begin
      CompareResult := _Compare(Node^.Key,Root^.Key);
      if CompareResult < 0 then
        Result := _Insert(Root^.Left,Node)
      else
        if CompareResult > 0 then
          Result := _Insert(Root^.Right,Node)
        else
          begin
            Result := False;
            DestroyNode(Node);
          end;
    end;
end;

function TBinaryTree.Search(Key: Pointer; var Data: Pointer): Boolean;
var
  Node: PTreeNode;
begin
  Node := _Search(FRoot,Key);
  Result := Node <> nil;
  if Result then
    Data := Node^.Data;
end;

function TBinaryTree.Search(Key: NativeInt; var Data: Pointer): Boolean;
begin
  Result := Search(PPtrInt(Key),Data);
end;

function TBinaryTree._Search(var Root: PTreeNode; Key: Pointer): PTreeNode;
var
  CompareResult: Integer;
begin
  Result := Root;
  if Root <> nil then
    begin
      CompareResult := _Compare(Key,Root^.Key);
      if CompareResult < 0 then
        Result := _Search(Root^.Left,Key)
      else
        if CompareResult > 0 then
          Result := _Search(Root^.Right,Key);
    end;
end;

procedure TBinaryTree.Iterate(Proc: TIteratorProc);
begin
  _Iterate(FRoot,Proc);
end;

procedure TBinaryTree._Iterate(var Root: PTreeNode; Proc: TIteratorProc);
begin
  if Root <> nil then
    begin
      _Iterate(Root^.Left,Proc);
      Proc(Root^.Key,Root^.Data);
      _Iterate(Root^.Right,Proc);
    end;
end;

procedure TBinaryTree.Clear;
begin
  _Clear(FRoot);
  FRecordCount := 0;
end;

function TBinaryTree._Count(Root: PTreeNode): Integer;
begin
  if Root = nil then
    Result := 0
  else
    Result := 1 + _Count(Root^.Left) + _Count(Root^.Right);
end;

function TBinaryTree.CalcRecordCount: Integer;
begin
  Result := _Count(FRoot);
end;

procedure TBinaryTree._Clear(var Root: PTreeNode);
begin
  if Root <> nil then
    begin
      _Clear(Root^.Left);
      _Clear(Root^.Right);
      DestroyNode(Root);
    end;
end;

function TBinaryTree.Remove(Key: Pointer): Boolean;
begin
  Result := _Remove(FRoot,Key);
  if Result then
    dec(FRecordCount);
end;

function TBinaryTree.Remove(Key: NativeInt): Boolean;
begin
  Result := Remove(PPtrInt(Key));
end;

function TBinaryTree._Remove(var Root: PTreeNode; Key: Pointer): Boolean;
var
  CompareResult: Integer;
  Q: PTreeNode;
begin
  if Root = nil then
    Result := False
  else
    begin
      CompareResult := _Compare(Key,Root^.Key);
      if CompareResult < 0 then
        Result := _Remove(Root^.Left,Key)
      else
        if CompareResult > 0 then
          Result := _Remove(Root^.Right,Key)
        else
          begin
            Result := True;
            if Root^.Left = nil then      // No left tree, move right tree up to replace node.
              begin
                Q := Root;
                Root := Root^.Right;
                DestroyNode(Q);
              end
            else
              if Root^.Right = nil then  // No right tree, move left tree up to replace node
                begin
                  Q := Root;
                  Root := Root^.Left;
                  DestroyNode(Q);
                end
              else
                begin
                  // Find immediate successor of root and store in Q
                  Q := Root^.Right;
                  while Q^.Left <> nil do
                    Q := Q^.Left;
                  // Copy successor data into root
                  Root^.Key := Q^.Key;
                  Root^.Data := Q^.Data;
                  // Remove the successor instead
                  Result := _Remove(Root^.Right,Q^.Key);
                end;
          end;
    end;
end;

{procedure TRBTree.RotateLeft(var Node: PTreeNode);
var
  Q: PTreeNode;
begin
  Q := Node^.Right;
  Node^.Right := Q^.Left;
  Q^.Left := Node;
  Node := Q;
end;

procedure TRBTree.RotateRight(var Node: PTreeNode);
var
  Q: PTreeNode;
begin
  Q := Node^.Left;
  Node^.Left := Q^.Right;
  Q^.Right := Node;
  Node := Q;
end;      }

end.

