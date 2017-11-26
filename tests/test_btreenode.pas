unit test_btreenode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, btree2;

type

  TTBTreeNode= class(TTestCase)
  protected
    Tree: TBTree;
    Node: TBTreeNode;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test;
  end;

implementation

procedure TTBTreeNode.Test;
var
  I: PtrInt;
begin
  for I := 1 to Tree.Order do
    PInteger(Node.Data[I])^ := I;

  for I := 1 to Tree.Order do
    AssertEquals(I,PInteger(Node.Data[I])^);

  for I := 0 to Tree.Order do
    Node.Next[I] := TBTreeNode(I+4);
  for I := 0 to Tree.Order do
    AssertEquals(I+4,PtrInt(Node.Next[I]));
end;

procedure TTBTreeNode.SetUp;
begin
  Tree := TBTree.Create(nil,4,6);
  Node := TBTreeNode.Create(Tree);
end;

procedure TTBTreeNode.TearDown;
begin
  Node.Free;
  Tree.Free;
end;

initialization
  RegisterTest(TTBTreeNode);
end.

