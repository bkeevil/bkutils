unit BTreeTestHelper;

{$mode objfpc}

interface

uses
  Classes, SysUtils, btreefile;

type
  TRec = record
    Key: Integer;
    Data: Integer;
  end;
  PRec = ^TRec;

  { TRandomArray }

  TRandomArray = class(TObject)
    private
      FArray: array of Integer;
      FCount: Integer;
      procedure FillSequential;
      function GetItem(Index: Integer): Integer;
      procedure SetCount(AValue: Integer);
      procedure SetItem(Index: Integer; AValue: Integer);
      procedure Shuffle(Repititions: Integer);
      procedure Swap(Index1,Index2: Integer);
      function IndexOf(AValue: Integer): Integer;
    public
      destructor Destroy; override;
      procedure FillRandom;
      procedure SaveState;
      procedure LoadState;
      procedure CheckForDuplicates;
      property Count: Integer read FCount write SetCount;
      property Items[Index: Integer]: Integer read GetItem write SetItem;
  end;

  { TBTreeTestHelper }

  TBTreeTestHelper = class(TObject)
    private
      FTree: TBTreeFileHandle;
    public
      constructor Create(ATree: TBTreeFileHandle);
      procedure Add(Start,Finish: Integer);
      procedure Remove(Start,Finish: Integer);
      procedure InsertSeq(Seq: TRandomArray);
      procedure SearchSeq(Seq: TRandomArray);
      procedure RemoveSeq(Seq: TRandomArray);

  end;

implementation

{ TRandomArray }

destructor TRandomArray.Destroy;
begin
  Count := 0;
  inherited Destroy;
end;

function TRandomArray.IndexOf(AValue: Integer): Integer;
begin
  for Result := 0 to FCount - 1 do
    if FArray[Result] = AValue then Exit;
  Result := -1;
end;

procedure TRandomArray.SetCount(AValue: Integer);
begin
  if FCount=AValue then Exit;
  FCount:=AValue;
  SetLength(FArray,Count);
end;

function TRandomArray.GetItem(Index: Integer): Integer;
begin
  Result := FArray[Index];
end;

procedure TRandomArray.SetItem(Index: Integer; AValue: Integer);
begin
  FArray[Index] := AValue;
end;

procedure TRandomArray.Shuffle(Repititions: Integer);
var
  I,A,B: Integer;
begin
  for I := 1 to Repititions do
    begin
      repeat
        A := Random(FCount);
        B := Random(FCount);
      until A <> B;
      Swap(A,B);
    end;
end;

procedure TRandomArray.Swap(Index1, Index2: Integer);
var
  T: Integer;
begin
  T := FArray[Index1];
  FArray[Index1] := FArray[Index2];
  FArray[Index2] := T;
end;

procedure TRandomArray.FillSequential;
var
  I: Integer;
begin
  for I := 0 to FCount -1 do
    FArray[I] := I;
end;

procedure TRandomArray.FillRandom;
begin
  FillSequential;
  Shuffle(FCount*2);
end;

procedure TRandomArray.SaveState;
var
  I: Integer;
  S: TFileStream;
begin
  S := TFileStream.Create('ArrayState.bin',fmCreate);
  try
    S.Write(FCount,SizeOf(FCount));
    for I := 0 to FCount - 1 do
      S.Write(FArray[I],SizeOf(Integer));
  finally
    S.Free;
  end;
end;

procedure TRandomArray.LoadState;
var
  I: Integer;
  X: Integer;
  S: TFileStream;
begin
  if FileExists('ArrayState.bin') then
    begin
      S := TFileStream.Create('ArrayState.bin',fmOpenRead);
      try
        S.Read(X,SizeOf(FCount));
        Count := X;
        for I := 0 to FCount - 1 do
          begin
            S.Read(X,SizeOf(Integer));
            FArray[I] := X;
          end;
      finally
        S.Free;
      end;
    end
  else
    begin
      FillRandom;
    end;
  CheckForDuplicates;
end;

procedure TRandomArray.CheckForDuplicates;
var
  X,Y: Integer;
begin
  for X := 0 to FCount - 2 do
    for Y := X+1 to FCount - 1 do
      if FArray[X] = FArray[Y] then
        raise Exception.Create('Duplicate entry in TRandomArray found');
end;

{ TBTreeTestHelper }

constructor TBTreeTestHelper.Create(ATree: TBTreeFileHandle);
begin
  FTree := ATree;
end;

procedure TBTreeTestHelper.Add(Start, Finish: Integer);
var
  X: Integer;
  R: TRec;
begin
  if Start < Finish then
    for X := Start to Finish do
      begin
        R.Key := X;
        R.Data := X;
        FTree.Insert(@R);
      end
  else
    for X := Start downto Finish do
      begin
        R.Key := X;
        R.Data := X;
        FTree.Insert(@R);
      end
end;

procedure TBTreeTestHelper.Remove(Start, Finish: Integer);
var
  X: Integer;
  R: TRec;
begin
  if Start < Finish then
    for X := Start to Finish do
      begin
        R.Key := X;
        FTree.Remove(@R);
      end
  else
    for X := Start downto Finish do
      begin
        R.Key := X;
        FTree.Remove(@R);
      end
end;

procedure TBTreeTestHelper.InsertSeq(Seq: TRandomArray);
var
  I: Integer;
  R: TRec;
  Result: Boolean;
begin
  for I := 0 to Seq.Count - 1 do
    begin
      R.Key := Seq.Items[I];
      R.Data := R.Key;
      Result := FTree.Insert(@R);
      Assert(Result=True);
    end;
end;

procedure TBTreeTestHelper.SearchSeq(Seq: TRandomArray);
var
  I: Integer;
  R: TRec;
  Result: Boolean;
begin
  for I := 0 to Seq.Count - 1 do
    begin
      R.Key := Seq.Items[I];
      Result := FTree.Search(@R);
      Assert(Result=True)
    end;
end;

procedure TBTreeTestHelper.RemoveSeq(Seq: TRandomArray);
var
  I: Integer;
  R: TRec;
begin
  for I := 0 to Seq.Count - 1 do
    begin
      R.Key := Seq.Items[I];
      if not FTree.Remove(@R) then
        raise Exception.CreateFmt('Failed at Index=%d Key=%d',[I,R.Key]);
    end;
end;

end.

