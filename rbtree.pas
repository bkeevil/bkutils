unit rbtree;
{ rbtree.pas - A unit that provides a red-black tree       Version 1.1
  Copyright (C) 2014  H. Bond Keevil

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

  For assistance or support: me@bondkeevil.ca }

{$mode objfpc}{$H+}

interface

type
  PTreeNode = ^TTreeNode;
  {$Z1}
  TTreeNodeColor = (ncRed, ncBlack);
  TDirection = (Right,Left);
  TTreeNode = record
    Data : Pointer;
    Color: TTreeNodeColor;
    Left, Right: PTreeNode;
  end;
  TIteratorProc = procedure (Data: Pointer; var Abort: Boolean) of object;
  TCompareFunc = function (Data1, Data2: Pointer): Integer of object;

  { TRBTree }

  TRBTree = class(TObject)
    private
      FDataSize    : Integer;
      FRecordCount : Integer;
      _Compare     : TCompareFunc;
      FAbort       : Boolean;
      FIndex       : Integer;
      FTemp        : Integer;
      FNode        : Pointer;
      procedure Rebalance(var U: PTreeNode; var TestColor: TTreeNodeColor; Dir: TDirection);
      procedure Rotate(var Node: PTreeNode; Dir: TDirection);
      procedure RotateLeft(var Node: PTreeNode);
      procedure RotateRight(var Node: PTreeNode);
      procedure SetColors(var Root: PTreeNode; RootColor, ChildColor: TTreeNodeColor);
      function _Count(Root: PTreeNode): Integer;
      function _Remove(var Root: PTreeNode; Key: Pointer; var TestColor: TTreeNodeColor): Boolean;
      function _Insert(var Root: PTreeNode; Node: PTreeNode; var ExcessRed: PTreeNode): Boolean;
      procedure _Iterate(var Root: PTreeNode; Proc: TIteratorProc);
      function _Search(var Root: PTreeNode; Key: Pointer): PTreeNode;
      procedure _Clear(var Root: PTreeNode);
      procedure IndexIterator(Data: Pointer; var Abort: Boolean);
      procedure _Walk(var Root: PTreeNode; Proc: TIteratorProc);
    protected
      function CreateNode(Data: Pointer): PTreeNode; virtual;
      procedure DestroyNode(var Node: PTreeNode); virtual;
    public
      FRoot: PTreeNode;
      constructor Create(Compare: TCompareFunc; ADataSize: Integer);
      destructor Destroy; override;

      function Insert(Data: Pointer): Boolean; overload;
      function Search(Data: Pointer): Boolean; overload;
      function Remove(Key: Pointer): Boolean; overload;
      procedure Iterate(Proc: TIteratorProc);
      procedure Walk(Proc: TIteratorProc);
      procedure Clear;
      function GetItem(Index: Integer): Pointer;
      function CalcRecordCount: Integer;
      property RecordCount: Integer read FRecordCount;
      property DataSize: Integer read FDataSize;
  end;

function IntegerCompareFunc(Data1, Data2: Pointer): Integer;
function PointerCompareFunc(Data1, Data2: Pointer): Integer;

implementation

// Overflow save integer compare
function IntegerCompareFunc(Data1, Data2: Pointer): Integer;
begin
  if PPtrInt(Data1) > PPtrInt(Data2) then
    Result := 1
  else
    if PPtrInt(Data1) < PPtrInt(Data2) then
      Result := -1
    else
      Result := 0;
end;

// Overflow save pointer compare that works with 32 or 64 bit systems
function PointerCompareFunc(Data1, Data2: Pointer): Integer;
begin
  if PtrUInt(Data1) > PtrUInt(Data2) then
    Result := 1
  else
    if PtrUInt(Data1) < PtrUInt(Data2) then
      Result := -1
    else
      Result := 0;
end;

constructor TRBTree.Create(Compare: TCompareFunc; ADataSize: Integer);
begin
  inherited Create;
  FDataSize := ADataSize;
  _Compare := Compare;
end;

destructor TRBTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TRBTree.CreateNode(Data: Pointer): PTreeNode;
begin
  Result := New(PTreeNode);
  Result^.Data := GetMem(DataSize);
  Move(Data^,Result^.Data^,DataSize);
  Result^.Left := nil;
  Result^.Right := nil;
  Result^.Color := ncRed;
end;

procedure TRBTree.DestroyNode(var Node: PTreeNode);
begin
  if Node^.Data <> nil then
    FreeMem(Node^.Data);
  Dispose(Node);
  Node := nil;
end;

function TRBTree.Insert(Data: Pointer): Boolean;
var
  ExcessRed: PTreeNode;
begin
  ExcessRed := nil;
  Result := _Insert(FRoot,CreateNode(Data),ExcessRed);
  if Result then inc(FRecordCount);
end;

function TRBTree._Insert(var Root: PTreeNode; Node: PTreeNode; var ExcessRed: PTreeNode): Boolean;
var
  CompareResult: Integer;
begin
  if Root = nil then
    begin
      Root := Node;
      Root^.Color := ncRed;
      ExcessRed := nil;
      Result := True;
    end
  else
    begin
      CompareResult := _Compare(Node^.Data,Root^.Data);
      if CompareResult < 0 then
        begin
          Result := _Insert(Root^.Left,Node,ExcessRed);

          if ExcessRed <> nil then
            if (Root^.Right <> nil) and (Root^.Right^.Color = ncRed) then
              SetColors(Root,ncRed,ncBlack)
            else
              if ExcessRed = Root^.Left^.Left then
                begin
                  RotateRight(Root);
                  SetColors(Root,ncRed,ncBlack);
                end
              else
                begin
                  RotateLeft(Root^.Left);
                  RotateRight(Root);
                  SetColors(Root,ncBlack,ncRed);
                end;

            if (Root^.Color = ncRed) and (Root^.Left^.Color = ncRed) then
              ExcessRed := Root^.Left
            else
              ExcessRed := nil;
        end
      else
        if CompareResult > 0 then
          begin
            Result := _Insert(Root^.Right,Node,ExcessRed);

            if ExcessRed <> nil then
              if (Root^.Left <> nil) and (Root^.Left^.Color = ncRed) then
                SetColors(Root,ncRed,ncBlack)
              else
                if ExcessRed = Root^.Right^.Right then
                  begin
                    RotateLeft(Root);
                    SetColors(Root,ncRed,ncBlack);
                  end
                else
                  begin
                    RotateRight(Root^.Right);
                    RotateLeft(Root);
                    SetColors(Root,ncBlack,ncRed);
                  end;

              if (Root^.Color = ncRed) and (Root^.Right^.Color = ncRed) then
                ExcessRed := Root^.Right
              else
                ExcessRed := nil;
          end
        else
          begin
            Result := False;
            ExcessRed := nil;
            DestroyNode(Node);
          end;
    end;
end;

procedure TRBTree.SetColors(var Root: PTreeNode; RootColor, ChildColor: TTreeNodeColor);
begin
  if Root^.Left <> nil then
    Root^.Left^.Color := ChildColor;
  if Root^.Right <> nil then
    Root^.Right^.Color := ChildColor;
  Root^.Color := RootColor;
end;

function TRBTree.Search(Data: Pointer): Boolean;
var
  Node: PTreeNode;
begin
  Node := _Search(FRoot,Data);
  Result := Node <> nil;
  if Result then
    Move(Node^.Data^,Data^,DataSize);
end;

function TRBTree._Search(var Root: PTreeNode; Key: Pointer): PTreeNode;
var
  CompareResult: Integer;
begin
  Result := Root;
  if Root <> nil then
    begin
      CompareResult := _Compare(Key,Root^.Data);
      if CompareResult < 0 then
        Result := _Search(Root^.Left,Key)
      else
        if CompareResult > 0 then
          Result := _Search(Root^.Right,Key);
    end;
end;

procedure TRBTree.Iterate(Proc: TIteratorProc);
begin
  FAbort := False;
  _Iterate(FRoot,Proc);
end;

procedure TRBTree.Walk(Proc: TIteratorProc);
begin
  FAbort := False;
  _Walk(FRoot,Proc);
end;

procedure TRBTree._Iterate(var Root: PTreeNode; Proc: TIteratorProc);
begin
  if (Root <> nil) and not FAbort then
    begin
      _Iterate(Root^.Left,Proc);
      Proc(Root^.Data,FAbort);
      _Iterate(Root^.Right,Proc);
    end;
end;

procedure TRBTree._Walk(var Root: PTreeNode; Proc: TIteratorProc);
begin
  if (Root <> nil) and not FAbort then
    begin
      _Walk(Root^.Left,Proc);
      _Walk(Root^.Right,Proc);
      Proc(Root^.Data,FAbort);
    end;
end;

procedure TRBTree.Clear;
begin
  _Clear(FRoot);
  FRecordCount := 0;
end;

procedure TRBTree.IndexIterator(Data: Pointer; var Abort: Boolean);
begin
  Abort := FTemp = FIndex;
  if Abort then
    FNode := Data
  else
    inc(FTemp);
end;

function TRBTree.GetItem(Index: Integer): Pointer;
begin
  FTemp := 0;
  FIndex := Index;
  FNode := nil;
  Iterate(@IndexIterator);
  Result := FNode;
end;

function TRBTree._Count(Root: PTreeNode): Integer;
begin
  if Root = nil then
    Result := 0
  else
    Result := 1 + _Count(Root^.Left) + _Count(Root^.Right);
end;

function TRBTree.CalcRecordCount: Integer;
begin
  Result := _Count(FRoot);
end;

procedure TRBTree._Clear(var Root: PTreeNode);
begin
  if Root <> nil then
    begin
      _Clear(Root^.Left);
      _Clear(Root^.Right);
      DestroyNode(Root);
    end;
end;

function TRBTree.Remove(Key: Pointer): Boolean;
var
  TestColor: TTreeNodeColor = ncRed;
begin
  Result := _Remove(FRoot,Key,TestColor);
  if Result then
    dec(FRecordCount);
end;

function TRBTree._Remove(var Root: PTreeNode; Key: Pointer; var TestColor: TTreeNodeColor): Boolean;
var
  CompareResult: Integer;
  Q: PTreeNode;
begin
  if Root = nil then
    begin
      Result := False;
      TestColor := ncRed;
    end
  else
    begin
      CompareResult := _Compare(Key,Root^.Data);
      if CompareResult < 0 then
        begin
          Result := _Remove(Root^.Left,Key,TestColor);
          if TestColor = ncBlack then
            Rebalance(Root,TestColor,Left);
        end
      else
        if CompareResult > 0 then
          begin
            Result := _Remove(Root^.Right,Key,TestColor);
            if TestColor = ncBlack then
              Rebalance(Root,TestColor,Right);
          end
        else        { Node is Found }
          begin

            if Root^.Left = nil then      // No left tree, move right tree up to replace node.
              begin
                Q := Root;
                Root := Root^.Right;
                if Root = nil then
                  TestColor := Q^.Color
                else
                  begin
                    TestColor := ncRed;
                    Root^.Color := ncBlack;
                  end;
                DestroyNode(Q);
                Result := True;
              end
            else
              if Root^.Right = nil then  // No right tree, move left tree up to replace node
                begin
                  Q := Root;
                  Root := Root^.Left;
                  if Root = nil then
                    TestColor := Q^.Color
                  else
                    begin
                      TestColor := ncRed;
                      Root^.Color := ncBlack;
                    end;
                  DestroyNode(Q);
                  Result := True;
                end
              else
                begin
                  // Find immediate successor of root and store in Q
                  Q := Root^.Right;
                  while Q^.Left <> nil do
                    Q := Q^.Left;
                  // Copy successor data into root
                  Move(Q^.Data^,Root^.Data^,DataSize);
                  // Remove the successor instead
                  Result := _Remove(Root^.Right,Q^.Data,TestColor);
                  Assert(Result = True);
                  if TestColor = ncBlack then
                    Rebalance(Root,TestColor,Right);
                end;
          end;
    end;
end;

procedure TRBTree.Rebalance(var U: PTreeNode; var TestColor: TTreeNodeColor; Dir: TDirection);
var   { Corrects red-black tree after deletion }
  W, X, Y: PTreeNode;       {U is parent of node at fault.  W is sibling of node at fault.  X,Y are W's kids }
begin
  if Dir = Left then
    begin
      W := U^.Right;  { Select Sibling and Kids }
      X := W^.Left;
      Y := W^.Right;
    end
  else
    begin
      W := U^.Left;
      X := W^.Right;
      Y := W^.Left;
    end;
  if W^.Color = ncBlack then  { Case 1 }
    if ((x = nil) or (X^.color = ncBlack)) and ((y = nil) or (Y^.Color = ncBlack)) then
      begin                   { Case 1a }
        TestColor := U^.Color;  { Remember U's Color }
        U^.Color := ncBlack;    { Color Flip }
        W^.Color := ncRed;
      end
    else
      if (Y <> nil) and (Y^.Color = ncRed) then { Case 1b }
        begin
          W^.Color := U^.Color;
          Rotate(U,Dir);              { Single Rotation }
          U^.Left^.Color := ncBlack;
          U^.Right^.Color := ncBlack;
          TestColor := ncRed;
        end
      else
        begin                  { Case 1c }
          X^.Color := U^.Color;
          U^.Color := ncBlack;
          if Dir = Left then
            RotateRight(U^.Right)
          else
            RotateLeft(U^.Left);
          Rotate(U,Dir);                 { Double Rotation }
          TestColor := ncRed;        { Terminates }
        end
  else
    begin { w is Red }
      W^.Color := U^.Color;             { Case 2 }
      U^.Color := ncRed;
      Rotate(U,Dir);
      if Dir = Left then  { Apply Case 1 }
        Rebalance(U^.Left,TestColor,Dir)
      else
        Rebalance(U^.Right,TestColor,Dir);
      TestColor := ncRed;          { Terminates }
    end;
end;

procedure TRBTree.Rotate(var Node: PTreeNode; Dir: TDirection);
begin
  if Dir = Left then
    RotateLeft(Node)
  else
    RotateRight(Node);
end;

procedure TRBTree.RotateLeft(var Node: PTreeNode);
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
end;

end.

