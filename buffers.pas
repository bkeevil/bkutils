unit Buffers;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  PBufferChunk = ^TBufferChunk;
  TBufferChunk = record
    Size: Integer;
    Data: Pointer;
    Next: PBufferChunk;
  end;

  { TBuffer }

  TBuffer = class(TObject)
    private
      FSize: Integer;
      Root: PBufferChunk;
      Pos: Integer;
      function _Peek(Node: PBufferChunk; LPos: Integer; Data: Pointer; Size: Integer): Integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure CopyFrom(ABuffer: TBuffer);
      procedure WriteBuffer(ABuffer: TBuffer);
      procedure Write(Data: Pointer; Size: Integer);
      function Read(Data: Pointer; Size: Integer): Integer;
      function Peek(Data: Pointer; Size: Integer): Integer;
      procedure Prepend(Data: Pointer; Size: Integer);
      procedure Clear;
      property Size: Integer read FSize;
  end;

  { TQueue }

  PQueueItem = ^TQueueItem;
  TQueueItem = record
    Item: Pointer;
    Next: PQueueItem;
  end;

  TQueue = class(TObject)
    private
      FFirst: PQueueItem;
      FCount: Word;
    public
      destructor Destroy; override;
      procedure Clear;
      procedure Push(Item: Pointer);      // Add item to back of queue
      procedure Prepend(Item: Pointer);   // Add item to front of queue
      function Pop: Pointer;              // Pull item from front of queue
      function Peek: Pointer;             // Retrieve item from front of queue but leave it in queue
      property Count: Word read FCount;
  end;

implementation

{ TBuffer }

constructor TBuffer.Create;
begin
  Root := nil;
  Pos := 0;
end;

destructor TBuffer.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TBuffer.CopyFrom(ABuffer: TBuffer);
//var
//  Data: Pointer;
//  Size: Integer;
begin
  Clear;
{  Size := ABuffer.Size;
  GetMem(Data,Size);
  try
    ABuffer.Peek(Data,Size);
    Write(Data,Size);
  finally
    FreeMem(Data,Size);
  end;}
  Root := New(PBufferChunk);
  // Initialize the new node
  Root^.Size := 0;
  Root^.Next := nil;
  // Place data in the node
  GetMem(Root^.Data,ABuffer.Size);
  ABuffer.Peek(Root^.Data,ABuffer.Size);
  Root^.Size := ABuffer.Size;
  FSize := ABuffer.Size;
end;

procedure TBuffer.WriteBuffer(ABuffer: TBuffer);
var
  Data: Pointer;
  Size: Integer;
begin
  Size := ABuffer.Size;
  GetMem(Data,Size);
  try
    Size := ABuffer.Read(Data,Size);
    Write(Data,Size);
  finally
    FreeMem(Data);
  end;
end;

// Return the total size of all unread data in the buffer
procedure TBuffer.Clear;
var
  Node: PBufferChunk;
begin
  while Root <> nil do
    begin
      Node := Root;
      Root := Node^.Next;
      FreeMem(Node^.Data);
      Dispose(Node);
    end;
  FSize := 0;
end;

// Writes data to the end of the buffer
procedure TBuffer.Write(Data: Pointer; Size: Integer);
var
  Cur: PBufferChunk;
begin
  if Size = 0 then Exit;
  // If the list is empty, create a root node
  if Root = nil then
    begin
      Root := New(PBufferChunk);
      Cur := Root;
    end
  else
    begin
      // Otherwise find the end of the list and create a node there
      Cur := Root;
      while Cur^.Next <> nil do
        Cur := Cur^.Next;
      Cur^.Next := New(PBufferChunk);
      Cur := Cur^.Next;
    end;

  // Initialize the new node
  Cur^.Size := 0;
  Cur^.Next := nil;
  // Place data in the node
  GetMem(Cur^.Data,Size);
  Move(Data^,Cur^.Data^,Size);
  Cur^.Size := Size;
  FSize := FSize + Size;
end;

// Reads data from the front of the buffer. Returns number of bytes read.
function TBuffer.Read(Data: Pointer; Size: Integer): Integer;
var
  Temp: PBufferChunk;
  SegPtr: Pointer;
  SegSize: Integer;
begin
  if (Root = nil) or (Size=0) then
    begin
      Result := 0;
      Exit;
    end;

  // Determine the start and size of the data segment to be read from this chunk
  SegPtr := Pointer(PtrUInt(Root^.Data) + Pos);
  SegSize := Root^.Size - Pos;
  if SegSize > Size then
    SegSize := Size;

  // Move the data from this chunk into the output buffer and advance the position counter
  Move(SegPtr^,Data^,SegSize);
  Pos := Pos+SegSize;
  FSize := FSize - SegSize;

  // If this buffer chunk has been fully read then dispose of it and advance root
  // to the next chunk
  if Pos=Root^.Size then
    begin
      FreeMem(Root^.Data);
      Temp := Root^.Next;
      Dispose(Root);
      Root := Temp;
      Pos := 0;
    end;

  // Increment the output data pointer by the number of bytes read and recursively read the next chunk
  Result := SegSize + Read(Pointer(PtrUInt(Data)+SegSize),Size-SegSize);
end;

// Reads data from the front of the buffer but doesn't remove it. Returns number of bytes read.
function TBuffer._Peek(Node: PBufferChunk; LPos: Integer; Data: Pointer; Size: Integer): Integer;
var
  SegPtr: Pointer;
  SegSize: Integer;
begin
  if (Node = nil) or (Size=0) then
    begin
      Result := 0;
      Exit;
    end;

  // Determine the start and size of the data segment to be read from this chunk
  SegPtr := Pointer(PtrUInt(Node^.Data) + LPos);
  SegSize := Node^.Size - LPos;
  if SegSize > Size then
    SegSize := Size;

  // Move the data from this chunk into the output buffer and advance the position counter
  Move(SegPtr^,Data^,SegSize);

  // Increment the output data pointer by the number of bytes read and recursively read the next chunk
  Result := SegSize + _Peek(Node^.Next,0,Pointer(PtrUInt(Data)+SegSize),Size-SegSize);
end;

// Reads data from the front of the buffer but doesn't remove it. Returns number of bytes read.
function TBuffer.Peek(Data: Pointer; Size: Integer): Integer;
begin
  Result := _Peek(Root,Pos,Data,Size);
end;

// Prepends data to the front of the buffer
procedure TBuffer.Prepend(Data: Pointer; Size: Integer);
var
  SegSize: Integer;
  SegPtr: Pointer;
  A: PBufferChunk;
begin
  if Size < 1 then Exit;
  // Fill in the part of the current root buffer chunk that has already been read
  if Pos > 0 then
    begin
      if Size < Pos then
        SegSize := Size
      else
        SegSize := Pos;
      SegPtr := Pointer(PtrUInt(Data) + Size - SegSize);
      System.Move(SegPtr^,Root^.Data^,SegSize);
      Pos := Pos - SegSize;
      Size := Size - SegSize;
      FSize := FSize + SegSize;
    end;
  if Pos <> 0 then
    raise Exception.Create('Pos should be zero');
  // Add remainder of data as new chunk before the root chunk
  if (Size > Pos) then
    begin
      New(A);
      A^.Size := Size - Pos;
      A^.Data := GetMem(A^.Size);
      System.Move(Data^,A^.Data^,A^.Size);
      FSize := FSize + A^.Size;
      A^.Next := Root;
      Root := A;
    end;
end;

{ TQueue }

destructor TQueue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TQueue.Clear;
var
  Ptr: PQueueItem;
begin
  while FFirst <> nil do
    begin
      Ptr := FFirst;
      FFirst := FFirst^.Next;
      Dispose(Ptr);
    end;
  FCount := 0;
end;

procedure TQueue.Push(Item: Pointer);
var
  Ptr,Obj: PQueueItem;
begin
  if FCount = $FFFF then
    raise Exception.Create('Queue is full');
  if FFirst = nil then
    begin
      Ptr := FFirst;
      New(FFirst);
      FFirst^.Next := Ptr;
      FFirst^.Item := Item;
    end
  else
    begin
      Ptr := FFirst;
      while Ptr^.Next <> nil do
        Ptr := Ptr^.Next;
      New(Obj);
      Ptr^.Next := Obj;
      Obj^.Next := nil;
      Obj^.Item := Item;
    end;
  inc(FCount);
end;

procedure TQueue.Prepend(Item: Pointer);
var
  Temp: PQueueItem;
begin
  if FCount = $FFFF then
    raise Exception.Create('Queue is full');
  Temp := FFirst;
  New(FFirst);
  FFirst^.Next := Temp;
  FFirst^.Item := Item;
  inc(FCount);
end;

function TQueue.Pop: Pointer;
var
  Ptr: PQueueItem;
begin
  if FFirst = nil then
    Result := nil
  else
    begin
      Result := FFirst^.Item;
      Ptr := FFirst;
      FFirst := FFirst^.Next;
      Dispose(Ptr);
      dec(FCount);
    end;
end;

function TQueue.Peek: Pointer;
begin
  if FFirst = nil then
    Result := nil
  else
    Result := FFirst^.Item;
end;

end.

