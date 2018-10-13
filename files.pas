unit Files;

{$mode objfpc}{$H+}

{$define DEBUG}
{$define BTree}
{$define Blowfish}
{$define IDEA}

interface

{ TODO : Need a memory file handle for temporary files }
{ TODO : A flat record file handle }

uses
  Classes, SysUtils, Logging, Crypto, DCPcrypt2;

const
  PAGE_SIZE                = 4096;  // Must be multiple of 128 for encryption to work
  DEFAULT_VERSION          = 1;
  MAX_FREE_PAGES           = 128;
  MAX_FILE_COUNT           = 256;
  MAX_META_PAGES           = 256;
  MAX_META_SIZE            = MAX_META_PAGES * PAGE_SIZE;
  MAX_FAT_PAGE_ENTRIES     = PAGE_SIZE div 4 - 1;
  DEFAULT_PACK_ITERATIONS  = 32;
  IMPORT_EXPORT_CHUNK_SIZE = 65536 * 2;       // Size of Import/Export memory buffer
  MIN_PROGRESS_SIZE        = 1024 * 1024 * 4; // Importing/Exporting files larger than this will trigger OnProgress event

  PAGEMAP_HEADER           = -MaxSmallInt;
  PAGEMAP_META             = -MaxSmallInt + 1;
  PAGEMAP_FREE             = -MaxSmallInt + 2;

  KEY_ENCRYPTION_CIPHER    = caRijndael;  // Must accept a 256 bit key
  KEY_ENCRYPTION_MODE      = cmCBC;
  KEY_ENCRYPTION_HASH      = haHaval256;  // Must use a 256 bit hash function
  PASSWORD_HASH            = haSHA1;      // Must be > 128 bits and different from encryption hash

type
  TPageNum                 = Cardinal;
  TPageIndex               = Cardinal;

  TFileDigest              = TBlock128;
  TFileKey                 = TBlock256;

  { TFileHeader }

  TFileHeader = packed record  { Size must = PAGE_SIZE }
    // First 64 bytes are not encrypted (except for the Key)
    Version       : Byte;                  //1
    Cipher        : TCipherAlgorithm;      //1
    Hash          : THashAlgorithm;        //1
    Unused1       : array[1..16-3] of Byte;
    Digest        : TFileDigest;           //16
    Key           : TFileKey;              //32
    //
    FreePageCount : 0..MAX_FREE_PAGES;
    MetaSize      : 0..MAX_META_SIZE;
    FreePages     : array[1..MAX_FREE_PAGES] of TPageNum;
    MetaPages     : array[1..MAX_META_PAGES] of TPageNum;
    FATPages      : array[1..MAX_FILE_COUNT] of TPageNum;
    FileSize      : array[1..MAX_FILE_COUNT] of TPageNum;
    Unused2       : array[1..443] of Byte;
  end;
  PFileHeader = ^TFileHeader;

  { TFATPage }

  TFATPage = record
    Pages: array[0..MAX_FAT_PAGE_ENTRIES - 1] of TPageNum;
    Next: TPageNum;
  end;
  PFATPage = ^TFATPage;

  { TBufferedPage }

  TBufferedPage = packed record
    PageNum: Cardinal;
    PageIdx: Cardinal;
    FileID: Byte;
    RefCount: Word;
    Modified: Boolean;
    Data: Pointer;
  end;
  PBufferedPage = ^TBufferedPage;

  // Forward Declarations
  TFileContainer       = class;
  TFileMetadata        = class;
  TFile                = class;
  TCustomFileHandle    = class;

  // Exceptions
  EFileError = class(Exception);

  // Events
  TFileOperation     = (foNone, foImport, foExport, foDelete, foDefrag, foEncrypt, foDecrypt);
  TFileProgressEvent = procedure (Sender: TObject; Operation: TFileOperation; Name: String; Position, Size: Integer) of object;
  TMetadataEvent     = procedure (Sender: TObject; Stream: TStream) of object;
  TFilePasswordEvent = procedure (Sender: TObject; var Password: String) of object;
  { TFileBuffer }

  TBufferMode = (bmSequential, bmRandomAccess);

  TFileBuffer = class(TObject)
    private
      FContainer  : TFileContainer;
      FFile       : TFile;
      FArray      : array of PBufferedPage;
      FCount      : Integer;
      FSize       : Integer;
      FMode       : TBufferMode;
      function GetPage(Index: Integer): PBufferedPage;
      procedure Delete(Index: Integer);
      function BinSearch(PageIdx: TPageIndex; var Location: Integer): Boolean;
      procedure Clear;
      function PageNumChanged(OldPageNum, NewPageNum: TPageNum): Boolean;
      procedure PageNumSwapped(PageNum1, PageNum2: TPageNum);

      procedure CheckOrder;
      procedure CheckRange;
      procedure DeleteUnusedBuffers;
      procedure SaveModifiedPages;
    public
      constructor Create(AFile: TFile);
      destructor Destroy; override;
      function Search(PageIdx: TPageIndex): PBufferedPage;
      function Insert(Page: PBufferedPage): Boolean;
      function Remove(PageIdx: TPageIndex): Boolean;
      procedure Truncate(PageIdx: TPageIndex);
      procedure Flush;
      procedure Validate;
      property Count: Integer read FCount;
      property Pages[Index: Integer]: PBufferedPage read GetPage; default;
      property Mode: TBufferMode read FMode write FMode;
  end;

  { TFileFAT }

  TFileFAT = class(TObject)
    private
      FContainer      : TFileContainer;
      FFile           : TFile;
      FPages          : array of PBufferedPage;
      FSize           : Integer;
      FFATPageCount   : TPageIndex;
      FFilePageCount  : TPageIndex;
      FLoaded         : Boolean;
      function CountZeroPages: Integer;
      function GetItem(Index: TPageIndex): TPageNum;
      procedure SetItem(Index: TPageIndex; AValue: TPageNum);
      function GetFATPage(Index: Integer): PBufferedPage;
      procedure PageNumSwapped(PageNum1, PageNum2: TPageNum);
      function PageNumChanged(OldPageNum, NewPageNum: TPageNum): Boolean;
      procedure Fetch;
      procedure Release;
      function FindEmptyPage(var PageIdx: TPageIndex): Boolean;
    protected
      property FATPages[Index: Integer]: PBufferedPage read GetFATPage;
      property FATPageCount: TPageIndex read FFATPageCount;
      procedure RemoveTrailingZeros;
    public
      constructor Create(AFile: TFile);
      destructor Destroy; override;
      //
      procedure Push(PageNum: TPageNum);
      procedure Pop;
      procedure Insert(PageIdx: TPageIndex; Value: TPageNum);
      procedure Delete(PageIdx: TPageIndex);
      procedure Flush;
      //
      property Items[Index: TPageIndex]: TPageNum read GetItem; default;
      property Count: TPageIndex read FFilePageCount;
  end;

  { TFileHandles }

  TFileHandles = class(TObject)
    private
      FArray : array of TCustomFileHandle;
      FSize  : Integer;
      FCount : Integer;
      function BinSearch(Handle: TCustomFileHandle; var Location: Integer): Boolean;
      procedure Delete(Location: Integer);
      function GetItem(Index: Integer): TCustomFileHandle;
      procedure InsertAt(Handle: TCustomFileHandle; Location: Integer);
      function Search(Handle: TCustomFileHandle): Integer;
    protected
      procedure NotifyRootChanged;
      procedure NotifySizeChanged;
      procedure NotifyDelete(PageIdx: TPageIndex);
    public
      destructor Destroy; override;
      procedure Insert(Handle: TCustomFileHandle);
      procedure Remove(Handle: TCustomFileHandle);
      procedure Close;
      property Count: Integer read FCount;
      property Items[Index: Integer]: TCustomFileHandle read GetItem; default;
  end;

  { TFile }
  {$Z1}
  TFileKind = (fkStream, fkPage, {$IFDEF btree}fkBTree, {$ENDIF}fkTable); // TODO: fkTable

  TFile = class(TLogObject)
    private
      FContainer   : TFileContainer;
      FFAT         : TFileFAT;
      FHandles     : TFileHandles;
      FBuffer      : TFileBuffer;
      FName        : String;
      FID          : Byte;
      FKind        : TFileKind;
      FRootNode    : TPageIndex;
      FRecordCount : Cardinal;
      FRecordSize  : Word;
      //
      procedure PageNumSwapped(PageNum1, PageNum2: TPageNum);
      procedure SetKind(AValue: TFileKind);
      procedure SetName(AValue: String);
      procedure SetRecordCount(AValue: Cardinal);
      procedure SetRecordSize(AValue: Word);
      procedure SetRootNode(AValue: TPageIndex);
      function GetPageCount: Cardinal;
      function GetSize: Cardinal;

      //
      function PageNumChanged(OldPageNum, NewPageNum: TPageNum): Boolean;
      //
      procedure LoadFromStream(Stream: TStream);
      procedure SaveToStream(Stream: TStream);
      procedure MetadataChanged;
    protected
      procedure SetSize(const AValue: Cardinal);
      //
      function Alloc: PBufferedPage;
      function Fetch(PageIdx: TPageIndex): PBufferedPage;
      procedure Release(var Page: PBufferedPage);
      //
      function Insert(PageIdx: TPageIndex): PBufferedPage;
      procedure Delete(PageIdx: TPageIndex);
      procedure Truncate(PageIdx: TPageIndex);
      //
      property FAT: TFileFAT read FFAT;
      property Buffer: TFileBuffer read FBuffer;
    public
      constructor Create(AContainer: TFileContainer; AName: String; AID: Byte; AKind: TFileKind = fkStream);
      constructor CreateFromStream(AContainer: TFileContainer; Stream: TStream);
      destructor Destroy; override;
      //
      function CreateHandle(AKind: TFileKind): TCustomFileHandle;
      procedure Flush;
      procedure Delete;
      //
      property Size: Cardinal read GetSize;
      property ID: Byte read FID;
      property Name: String read FName write SetName;
      property Kind: TFileKind read FKind write SetKind;
      property RootNode: TPageIndex read FRootNode write SetRootNode;
      property RecordCount: Cardinal read FRecordCount write SetRecordCount;
      property RecordSize: Word read FRecordSize write SetRecordSize;
      property PageCount: Cardinal read GetPageCount;
      property Handles: TFileHandles read FHandles;
  end;

  { TFiles }

  TFiles = class(TObject)
    private
      FContainer: TFileContainer;
      FArray: array of TFile;
      FSize: Integer;
      FCount: SmallInt;
      function GetItem(Index: Integer): TFile;
      function BinSearch(Name: String; var Location: Integer): Boolean;
      procedure InsertAt(AFile: TFile; Location: Integer);
      procedure Delete(AFile: TFile);
      procedure Delete(Location: Integer);
      function PageNumChanged(OldPageNum, NewPageNum: Cardinal): Boolean;
      procedure PageNumSwapped(PageNum1, PageNum2: TPageNum);
      procedure LoadFromStream(Stream: TStream);
      procedure SaveToStream(Stream: TStream);
    protected
      function GetID: Byte;
      function Insert(Name: String; ID: Byte; Kind: TFileKind): TFile; overload;
      function Insert(AFile: TFile): Integer; overload;
      function Remove(Name: String): Boolean; overload;
      function Remove(AFile: TFile): Boolean; overload;
      function Remove(AID: Byte): Boolean; overload;
      procedure Rename(OldName, NewName: String);
      procedure Clear;
      procedure MetadataChanged;
      procedure CloseHandles;
    public
      destructor Destroy; override;
      procedure Flush;
      function Find(ID: Byte): Integer; overload;
      function Find(Name: String): TFile; overload;
      function Find(AFile: TFile): Integer; overload;
      property Count: SmallInt read FCount;
      property Items[Index: Integer]: TFile read GetItem; default;
  end;

  { TCustomFileHandle }

  TCustomFileHandle = class(TLogObject)
    private
      FContainer : TFileContainer;
      FFile      : TFile;
      function GetID: Byte;
      function GetPageCount: Cardinal;
      function GetSize: Cardinal;
    protected
      FLast : PBufferedPage;
      procedure SetSize(const AValue: Cardinal);
      procedure SizeChanged; virtual;
      procedure ReleaseBuffer; virtual;
      procedure DeleteNotification(PageIdx: TPageIndex); virtual;
    public
      constructor Create(AFile: TFile); virtual;
      destructor Destroy; override;
      //
      procedure Flush; virtual;
      //
      property Container: TFileContainer read FContainer;
      property _File: TFile read FFile;
      property ID: Byte read GetID;
      property Size: Cardinal read GetSize;
      property PageCount: Cardinal read GetPageCount;
  end;

  { TPageFileHandle }

  TPageFileHandle = class(TCustomFileHandle)
    public
      constructor Create(AFile: TFile); override;
      function Fetch(PageIdx: TPageIndex): PBufferedPage;
      procedure Release(var Page: PBufferedPage);
      function Alloc: PBufferedPage;
      procedure Truncate(PageIdx: TPageIndex);
      function Insert(PageIdx: TPageIndex): PBufferedPage;
      procedure Delete(PageIdx: TPageIndex); overload;
      procedure Delete(var Page: PBufferedPage); overload;
  end;

  { TStreamFileHandle }

  TStreamFileHandle = class(TCustomFileHandle)
    private
      FPosition: Cardinal;
    protected
      procedure SizeChanged; override;
    public
      function Read(var Buffer; Count: Integer): Integer; overload;
      function Read(Length: Integer): String; overload;
      function Write(const Buffer; Count: Integer): Integer; overload;
      function Write(Str: String): Integer; overload;
      procedure Seek(Position: Cardinal);
      procedure Truncate(Position: Cardinal);
      property Position: Cardinal read FPosition write Seek;
  end;

  { TFreePages }

  TFreePages = class(TObject)
    private
      FContainer: TFileContainer;
      function GetCount: Integer;
      function GetItem(Index: Integer): TPageNum;
      function BinSearch(Key: TPageNum; var Location: Integer): Boolean;
      procedure InsertAt(PageNum: TPageNum; Location: Integer);
      procedure DeleteAt(Location: Integer);
      function PageNumChanged(OldPageNum, NewPageNum: TPageNum): Boolean;
      procedure PageNumSwapped(PageNum1, PageNum2: TPageNum);
    public
      procedure Insert(PageNum: TPageNum);
      function Remove(PageNum: TPageNum): Boolean;
      procedure Truncate(PageNum: TPageNum);
      function GetLowest: TPageNum;
      function CheckOrder: Boolean;
      property Count: Integer read GetCount;
      property Items[Index: Integer]: TPageNum read GetItem; default;
  end;

  { TPageMap }
  // Values are a PAGEMAP_ constant or:
  // if Value is positive it's a fileid,
  // if -ve then it's a FAT page for the fileid.
  // If zero then the page has been lost track of and it's an error.
  TPageMap = class(TLogObject)
    private
      FContainer: TFileContainer;
      FArray : array of SmallInt;
      FCount : Integer;
      FResult: Boolean;
      function GetPage(PageNum: TPageNum): SmallInt;
      procedure SetPage(PageNum: TPageNum; AValue: SmallInt);
      procedure SetCount(AValue: Integer);
      procedure Clear;
    public
      constructor Create(Container: TFileContainer);
      destructor Destroy; override;
      procedure Build;
      function Validate: Boolean;
      property Count: Integer read FCount write SetCount;
      property Values[PageNum: TPageNum]: SmallInt read GetPage write SetPage;
  end;

  { TFileMetadata }

  TFileMetadata = class(TLogObject)
    private
      FContainer: TFileContainer;
      FChanged: Boolean;
      function GetSize: Cardinal;
      function GetPage(Index: Integer): TPageNum;
      function GetPageCount: Integer;
      function PageNumChanged(OldPageNum, NewPageNum: TPageNum): Boolean;
      procedure PageNumSwapped(PageNum1, PageNum2: TPageNum);
    protected
      procedure SaveFromStream(Stream: TStream);
      procedure LoadToStream(Stream: TStream);
    public
      constructor Create;
      property Pages[Index: Integer]: TPageNum read GetPage;
      property PageCount: Integer read GetPageCount;
      property Size: Cardinal read GetSize;
      property Changed: Boolean read FChanged write FChanged;
  end;

  { TBasicPageFile }

  {TBasicPageFile = class(TLogObject)
    private
      FActive: Boolean;
      FFilename: String;
      FHandle: TFileHandle;
      FPageCount: Integer;
      FBuffer: PBufferedPage;
      //
      FBeforeOpen: TNotifyEvent;
      FAfterOpen: TNotifyEvent;
      FBeforeClose: TNotifyEvent;
      FAfterClose: TNotifyEvent;
      FOnReadData: TNotifyEvent;
      FOnWriteData: TNotifyEvent;
    protected
      procedure Replace(Source, Target: TPageNum); virtual;
      procedure Swap(PageNum1, PageNum2: TPageNum); virtual;
      procedure Truncate(PageNum: TPageNum); virtual;
      //
      function Fetch(PageNum: TPageNum): PBufferedPage; virtual;
      function Alloc: PBufferedPage; virtual;
      function Release(var Page: PBufferedPage); virtual;
      //
      procedure ReadData(PageNum: TPageNum; var Data: Pointer); virtual;
      procedure WriteData(PageNum: TPageNum; const Data: Pointer); virtual;
    public
      constructor Create;
      destructor Destroy; override;
      //
      procedure Open; virtual;
      procedure Close; virtual;
      //
      property Active: Boolean read FActive write SetActive;
      property Filename: String read FFilename write SetFilename;
      property Handle: TFileHandle read FHandle;
      property PageCount: Cardinal read FPageCount;
  end;       }

  { TFileContainer }

  TFileContainer = class(TLogObject)
    private
      FActive: Boolean;
      FFilename: String;
      FPageCount: Cardinal;
      FAutoPack: Boolean;
      FDeletingPage: TPageNum;
      FPassword: String;
      FCipher: TDCP_BlockCipher;
      FKey: TFileKey;
      FEncrypting: Boolean;   // TODO: Get rid of this and use FOperation instead
      FHandle: THandle;
      FFreePages: TFreePages;
      FMetadata: TFileMetadata;
      FFiles: TFiles;
      FPageMap: TPageMap;
      FOperation: TFileOperation;
      FOnProgress: TFileProgressEvent;
      FOnLoadMetadata: TMetadataEvent;
      FOnSaveMetadata: TMetadataEvent;
      FOnGetPassword: TFilePasswordEvent;
      //
      function Login: Boolean;
      function CheckPassword(AValue: String): Boolean;
      procedure SetPassword(AValue: String);
      function PasswordRequired: Boolean;
      //
      function GetCipherAlgorithm: TCipherAlgorithm;
      function GetEncrypted: Boolean;
      procedure GenerateCipherKey;
      procedure InitCipherKey;
      procedure LoadCipherKey;
      procedure SaveCipherKey;
      //
      procedure DoProgress(AName: String; APosition, ASize: Cardinal);
      function GetFileSize: Cardinal;
      function GetVersion: Byte;
      procedure Swap(Source, Target: PBufferedPage);
      procedure NotifyPageSwap(PageNum1, PageNum2: TPageNum);
      procedure SetActive(AValue: Boolean);
      procedure CreateHeader;
      procedure ReadHeader;
      procedure WriteHeader;
      procedure SaveMetadata;
      procedure LoadMetadata;
      //
      procedure DeleteTrailingFreePages;
      procedure Replace(Source, Target: TPageNum);
    protected
      FHeader: TFileHeader;
      procedure ReadData(PageNum: TPageNum; Data: Pointer);
      procedure WriteData(PageNum: TPageNum; Data: Pointer);
      procedure Truncate(PageNum: TPageNum);
      procedure Delete(PageNum: TPageNum); overload;
      procedure Delete(var Page: PBufferedPage); overload;
      procedure NotifyPageNumChanged(OldPageNum,NewPageNum: TPageNum);
      procedure Release(var Page: PBufferedPage);
      function Fetch(PageNum: TPageNum): PBufferedPage;
      function Alloc: PBufferedPage;
      procedure Dispose(var Page: PBufferedPage);
      procedure EncryptPage(PageNum: Cardinal; Data: Pointer);
      procedure DecryptPage(PageNum: Cardinal; Data: Pointer);
      //
      procedure ClearMetadata; virtual;
      procedure DoLoadMetadata(Stream: TStream); virtual;
      procedure DoSaveMetadata(Stream: TStream); virtual;
      //
      function BeginOperation(Operation: TFileOperation): Boolean;
      procedure EndOperation;
      //
      property Cipher: TDCP_BlockCipher read FCipher;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Open;
      procedure Close;
      function CreateFile(Filename: String; Kind: TFileKind = fkStream): TCustomFileHandle;
      function OpenFile(Filename: String): TCustomFileHandle; overload;
      function OpenFile(Filename: String; Kind: TFileKind): TCustomFileHandle; overload;
      function FileExists(Filename: String): Boolean;
      procedure RenameFile(OldName, NewName: String);
      function DeleteFile(Filename: String): Boolean;
      procedure ImportFile(Stream: TStream; Filename: String);
      procedure ExportFile(Stream: TStream; Filename: String);
      procedure Flush;
      procedure Pack(MaxIterations: Byte = DEFAULT_PACK_ITERATIONS);
      procedure Defrag;
      procedure Encrypt(Password: String; Algorithm: TCipherAlgorithm = caTwofish);
      procedure Decrypt;
      //
      property Active: Boolean read FActive write SetActive;
      property AutoPack: Boolean read FAutoPack write FAutoPack;
      property Encrypted: Boolean read GetEncrypted;
      property CipherAlgorithm: TCipherAlgorithm read GetCipherAlgorithm;
      property Filename: String read FFilename write FFilename;
      property Password: String read FPassword write SetPassword;
      property Version: Byte read GetVersion;
      property Operation: TFileOperation read FOperation;
      property PageCount: Cardinal read FPageCount;
      property FileSize: Cardinal read GetFileSize;
      property Files: TFiles read FFiles;
      property OnLoadMetadata: TMetadataEvent read FOnLoadMetadata write FOnLoadMetadata;
      property OnSaveMetadata: TMetadataEvent read FOnSaveMetadata write FOnSaveMetadata;
      property OnProgress: TFileProgressEvent read FOnProgress write FOnProgress;
      property OnGetPassword: TFilePasswordEvent read FOnGetPassword write FOnGetPassword;
      // For Testing Purposes
      {$IFDEF DEBUG}
      procedure Validate;
      procedure ValidateFreePages;
      procedure ValidatePageMap;
      procedure ValidateBuffers;
      property FreePages: TFreePages read FFreePages;
      property Header: TFileHeader read FHeader;
      property Metadata: TFileMetadata read FMetadata;
      property PageMap: TPageMap read FPageMap;
      {$ENDIF}
  end;

implementation

uses
  {$IFDEF Unix}
  BaseUnix,
  {$ENDIF}
  {$IFDEF Windows}
  Windows,
  {$ENDIF}
  {$IFDEF btree}
  btreefile,
  {$ENDIF}
  Rand,
  ErrorMsg;

type
  TByteArray = array[0..PAGE_SIZE] of Byte;
  TBTreeHack = class(TBTreeFileHandle);

var
  ZERO_DIGEST: TFileDigest = (0,0,0,0);

{ TPageMap }

constructor TPageMap.Create(Container: TFileContainer);
begin
  inherited Create;
  Log.Name := 'PageMap';
  FContainer := Container;
  {$IFDEF DEBUG}Log.Filter := [mtInfo,mtError,mtDebug];{$ELSE}Log.Filter := [mtInfo,mtError];{$ENDIF}
end;

destructor TPageMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TPageMap.Clear;
begin
  SetLength(FArray,0);
  FCount := 0;
end;

procedure TPageMap.Build;
var
  X,Y: Integer;
  PageNum: TPageNum;
  F: TFile;
begin
  Clear;
  if FContainer.PageCount > 0 then
    begin
      // Set page count and zero array
      Count := FContainer.PageCount;
      for X := 0 to Count - 1 do
        FArray[X] := 0;
      // First page is always the file header
      Values[0] := PAGEMAP_HEADER;
      // Add free page list to the page map
      for X := 1 to FContainer.FreePages.Count do
        begin
          PageNum := FContainer.FreePages[X];
          if PageNum < Count then                 // Free page list might contain pages beyond end of file
            Values[PageNum] := PAGEMAP_FREE;
        end;
      // Add Meta Pages
      for X := 1 to FContainer.Metadata.PageCount do
        Values[FContainer.Metadata.Pages[X]] := PAGEMAP_META;
      // Add FAT Pages
      for X := 0 to FContainer.Files.Count - 1 do
        begin
          F := FContainer.Files[X];
          for Y := 0 to F.FAT.Count - 1 do
            Values[F.FAT.Items[Y]] := X+1;
          for Y := 0 to F.FAT.FATPageCount - 1 do
            Values[F.FAT.FATPages[Y]^.PageNum] := -X - 1;
        end;
    end;
end;

function TPageMap.Validate: Boolean;
var
  X: Integer;
begin
  FResult := True;
  for X := 0 to FCount - 1 do
    if FArray[X] = 0 then
      begin
        FResult := False;
        Log.Send(mtError,'Pagemap Validation Failure: Lost track of page %d',[X]);
      end;
  Result := FResult;
end;

function TPageMap.GetPage(PageNum: TPageNum): SmallInt;
begin
  Assert(PageNum < FCount);
  Result := FArray[PageNum];
end;

procedure TPageMap.SetPage(PageNum: TPageNum; AValue: SmallInt);
begin
  Assert(PageNum < FCount);
  Assert(FArray <> nil);
  if FArray[PageNum] <> 0 then
    begin
      FResult := False;
      Log.Send(mtError,'Pagemap Validation Failure: Page %d is in two places',[PageNum]);
    end;
  FArray[PageNum] := AValue;
end;

procedure TPageMap.SetCount(AValue: Integer);
begin
  if FCount=AValue then Exit;
  FCount:=AValue;
  SetLength(FArray,FCount);
end;

{ TFiles }

destructor TFiles.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TFiles.Clear;
var
  I: Integer;
begin
  CloseHandles;
  for I := FCount - 1 downto 0 do
    FArray[I].Destroy;
  SetLength(FArray,0);
  FSize := 0;
  FCount := 0;
end;

procedure TFiles.MetadataChanged;
begin
  FContainer.FMetadata.FChanged := True;
end;

procedure TFiles.LoadFromStream(Stream: TStream);
var
  I: Integer;
  C: SmallInt;
  S: Integer;
begin
  Clear;
  S := Stream.Size - Stream.Position;
  if S >= SizeOf(C) then
    Stream.Read(C,SizeOf(C))
  else
    C := 0;
  for I := 0 to C - 1 do
    TFile.CreateFromStream(FContainer,Stream);
end;

procedure TFiles.SaveToStream(Stream: TStream);
var
  I: Integer;
begin
  Stream.Position := 0;
  Stream.Write(FCount,SizeOf(FCount));
  for I := 0 to FCount - 1 do
    FArray[I].SaveToStream(Stream);
end;

function TFiles.Find(Name: String): TFile;
var
  Location: Integer;
begin
  if BinSearch(Name,Location) then
    Result := FArray[Location]
  else
    Result := nil;
end;

function TFiles.Find(ID: Byte): Integer;
begin
  for Result := 0 to FCount - 1 do
    if FArray[Result].ID = ID then Exit;
  Result := -1;
end;

function TFiles.Find(AFile: TFile): Integer;
begin
  for Result := 0 to FCount - 1 do
    if FArray[Result] = AFile then Exit;
  Result := -1;
end;

function TFiles.BinSearch(Name: String; var Location: Integer): Boolean;
var
  First, Last, Mid: Integer;
  Res: Integer;
begin
  Result := False;
  Location := 0;
  if FCount > 0 then
    begin
      First := 0;
      Last := FCount - 1;
      while First <= Last do
        begin
          Mid := (First + Last) div 2;
          Res := CompareText(Name,FArray[Mid].Name);
          if Res > 0 then
            First := Mid + 1
          else
            if Res < 0 then
              Last := Mid - 1
            else
              begin
                Location := Mid;
                Result := True;
                Exit;
              end;
        end;
      Location := First;
    end;
end;

function TFiles.GetItem(Index: Integer): TFile;
begin
  if Index >= FCount then
    raise EFileError.Create('Index out of range');
  Result := FArray[Index];
end;

function TFiles.Insert(AFile: TFile): Integer;
begin
  Result := -1;
  if not Assigned(AFile) then Exit;
  if not BinSearch(AFile.Name,Result) then
    begin
      InsertAt(AFile,Result);
      MetadataChanged;
    end;
end;

function TFiles.Insert(Name: String; ID: Byte; Kind: TFileKind): TFile;
var
  Location: Integer;
begin
  Result := nil;
  if not BinSearch(Name,Location) then
    begin
      Result := TFile.Create(FContainer,Name,ID,Kind);
      InsertAt(Result,Location);
    end;
end;

procedure TFiles.InsertAt(AFile: TFile; Location: Integer);
var
  X: Integer;
begin
  inc(FCount);
  if FCount > FSize then
    begin
      FSize := FSize + 8;
      SetLength(FArray,FSize);
    end;
  for X := FCount - 2 downto Location do
    FArray[X+1] := FArray[X];
  FArray[Location] := AFile;
  MetadataChanged;
end;

function TFiles.GetID: Byte;
var
  X: Integer;
begin
  for X := 1 to MAX_FILE_COUNT do
    if Find(X) = -1 then
      begin
        Result := X;
        Exit;
      end;
  raise EFileError.Create('File table is full');
end;

function TFiles.Remove(Name: String): Boolean;
var
  Location: Integer;
begin
  Result := BinSearch(Name,Location);
  if Result then
    begin
      FArray[Location].Destroy;
      //Delete(Location); { Called by Destroy }
    end;
end;

function TFiles.Remove(AFile: TFile): Boolean;
var
  Location: Integer;
begin
  Result := Assigned(AFile);
  if Result then
    begin
      Location := Find(AFile);
      if Location > -1 then
        begin
          FArray[Location].Destroy;
          //Delete(Location); { Called by Destroy }
        end
      else
        Result := False;
    end;
end;

function TFiles.Remove(AID: Byte): Boolean;
var
  Location: Integer;
begin
  Result := (AID > 0) and (AID <= MAX_FILE_COUNT);
  if Result then
    begin
      Location := Find(AID);
      if Location > -1 then
        begin
          FArray[Location].Destroy;
          //Delete(Location); { Called by Destroy }
        end;
    end;
end;

procedure TFiles.Delete(AFile: TFile);
var
  I: Integer;
begin
  I := Find(AFile);
  if I > -1 then
    Delete(I);
end;

procedure TFiles.Delete(Location: Integer);
var
  I: Integer;
begin
  if (Location < 0) or (Location >= FCount) then
    raise EFileError.Create('Index out of range');
  for I := Location to FCount - 2 do
    FArray[I] := FArray[I+1];
  dec(FCount);
  MetadataChanged;
end;

// Renames a virtual file.  Throws an exception if OldName not found or if NewName already exists.
procedure TFiles.Rename(OldName, NewName: String);
var
  I, Location: Integer;
  F: TFile;
begin
  if Find(NewName) <> nil then
    raise EFileError.Create('A file named '+NewName+' already exists');
  if BinSearch(OldName,Location) then
    begin
      F := FArray[Location];
      for I := Location to FCount - 2 do
        FArray[I] := FArray[I+1];
      dec(FCount);
      BinSearch(NewName,Location);
      F.FName := NewName;
      F.MetadataChanged;
      InsertAt(F,Location);
      MetadataChanged;
    end
  else
    raise EFileError.Create('File '+OldName+' not found');
end;

function TFiles.PageNumChanged(OldPageNum, NewPageNum: Cardinal): Boolean;
var
  I: Integer;
  F: TFile;
begin
  for I := 0 to FCount - 1 do
    begin
      F := FArray[I];
      if Assigned(F) and F.PageNumChanged(OldPageNum,NewPageNum) then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

procedure TFiles.PageNumSwapped(PageNum1, PageNum2: TPageNum);
var
  I: Integer;
  F: TFile;
begin
  for I := 0 to FCount - 1 do
    begin
      F := FArray[I];
      if Assigned(F) then
        F.PageNumSwapped(PageNum1,PageNum2);
    end;
end;

procedure TFiles.Flush;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FArray[I].Flush;
end;

procedure TFiles.CloseHandles;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    FArray[I].Handles.Close;
end;

{ TFile }

constructor TFile.Create(AContainer: TFileContainer; AName: String; AID: Byte; AKind: TFileKind);
begin
  inherited Create;
  {$IFNDEF DEBUG}Log.Filter := [mtInfo,mtError];{$ENDIF}
  FContainer := AContainer;
  FName := AName;
  FID := AID;
  FKind := AKind;
  FFAT := TFileFAT.Create(Self);
  FHandles := TFileHandles.Create;
  FBuffer := TFileBuffer.Create(Self);
  if FContainer.FFiles.Insert(Self) = -1 then
    raise EFileError.Create('Duplicate Filename '+FName);
end;

constructor TFile.CreateFromStream(AContainer: TFileContainer; Stream: TStream);
begin
  inherited Create;
  {$IFNDEF DEBUG}Log.Filter := [mtInfo,mtError];{$ENDIF}
  FContainer := AContainer;
  LoadFromStream(Stream);
  FFAT := TFileFAT.Create(Self);
  FHandles := TFileHandles.Create;
  FBuffer := TFileBuffer.Create(Self);
  if FContainer.FFiles.Insert(Self) = -1 then
    raise EFileError.Create('Duplicate Filename '+FName)
  else
    FFAT.Fetch;
end;

destructor TFile.Destroy;
begin
  Flush;
  FHandles.Destroy;
  FContainer.FFiles.Delete(Self);
  FBuffer.Destroy;
  FFAT.Destroy;
  inherited Destroy;
end;

function TFile.CreateHandle(AKind: TFileKind): TCustomFileHandle;
begin
  case AKind of
    fkStream : Result := TStreamFileHandle.Create(Self);
    fkPage   : Result := TPageFileHandle.Create(Self);
    fkBTree  : Result := TBTreeFileHandle.Create(Self);
  end;
end;

procedure TFile.LoadFromStream(Stream: TStream);
var
  C: Char = #0;
  L: Word = 0;
  I: Integer;
begin
  Stream.Read(FID,SizeOf(FID));
  Stream.Read(FKind,SizeOf(FKind));
  if FKind = fkBTree then
    Stream.Read(FRootNode,SizeOf(FRootNode))
  else
    FRootNode := 0;
  if FKind in [fkBTree,fkTable] then
    begin
      Stream.Read(FRecordCount,SizeOf(FRecordCount));
      Stream.Read(FRecordSize,SizeOf(FRecordSize));
    end
  else
    FRecordCount := 0;
  FName := '';
  Stream.Read(L,SizeOf(Word));
  for I := 1 to L do
    begin
      Stream.Read(C,1);
      FName := FName + C;
    end;
end;

procedure TFile.SaveToStream(Stream: TStream);
var
  C: Char;
  L: Word;
  I: Integer;
begin
  Stream.Write(FID,SizeOf(FID));
  Stream.Write(FKind,SizeOf(FKind));
  if FKind = fkBTree then
    Stream.Write(FRootNode,SizeOf(FRootNode));
  if FKind in [fkBTree,fkTable] then
    begin
      Stream.Write(FRecordCount,SizeOf(FRecordCount));
      Stream.Write(FRecordSize,SizeOf(FRecordSize));
    end;
  L := Length(FName);
  Stream.Write(L,SizeOf(L));
  for I := 1 to L do
    begin
      C := FName[I];
      Stream.Write(C,1);
    end;
end;

procedure TFile.MetadataChanged;
begin
  FContainer.FMetadata.FChanged := True;
end;

function TFile.GetSize: Cardinal;
begin
  Result := FContainer.FHeader.FileSize[ID];
end;

procedure TFile.SetSize(const AValue: Cardinal);
begin
  FContainer.FHeader.FileSize[ID] := AValue;
  FHandles.NotifySizeChanged;
end;

procedure TFile.SetName(AValue: String);
begin
  if FName = AValue then Exit;
  FContainer.FFiles.Rename(FName,AValue);
end;

function TFile.GetPageCount: Cardinal;
begin
  Result := FFAT.Count;
end;

procedure TFile.SetRecordCount(AValue: Cardinal);
begin
  if FRecordCount=AValue then Exit;
  FRecordCount:=AValue;
  MetadataChanged;
end;

procedure TFile.SetRecordSize(AValue: Word);
begin
  if FRecordSize=AValue then Exit;
  FRecordSize:=AValue;
  MetadataChanged;
end;

procedure TFile.SetRootNode(AValue: TPageIndex);
begin
  if FRootNode=AValue then Exit;
  FRootNode:=AValue;
  MetadataChanged;
  FHandles.NotifyRootChanged;
end;

procedure TFile.SetKind(AValue: TFileKind);
begin
  if FKind=AValue then Exit;
  FKind:=AValue;
  MetadataChanged;
end;

function TFile.PageNumChanged(OldPageNum, NewPageNum: TPageNum): Boolean;
begin
  Result := False;
  if FBuffer.PageNumChanged(OldPageNum, NewPageNum) then
    begin
      //if AutoFlush then FBuffer.Flush;
      Log.Send(mtDebug,'PageNumChanged(%d,%d) handled by TFileBuffer(%d)',[OldPageNum,NewPageNum,ID]);
      Result := True;
    end;
  if FFAT.PageNumChanged(OldPageNum, NewPageNum) then
    begin
      //if AutoFlush then FFAT.Flush;
      Log.Send(mtDebug,'PageNumChanged(%d,%d) handled by TFileFAT(%d)',[OldPageNum,NewPageNum,ID]);
      Result := True;
    end;
end;

procedure TFile.PageNumSwapped(PageNum1,PageNum2: TPageNum);
begin
  FBuffer.PageNumSwapped(PageNum1,PageNum2);
  FFAT.PageNumSwapped(PageNum1,PageNum2);
end;

function TFile.Alloc: PBufferedPage;
begin
  Result := FContainer.Alloc;
  Result^.FileID := ID;
  if (Kind = fkBTree) and FFAT.FindEmptyPage(Result^.PageIdx) then
    FFAT.SetItem(Result^.PageIdx,Result^.PageNum)
  else
    begin
      Result^.PageIdx := PageCount;
      FFat.Push(Result^.PageNum);
    end;
  if Kind <> fkStream then
    SetSize(Size + PAGE_SIZE);
  FBuffer.Insert(Result);
end;

function TFile.Fetch(PageIdx: TPageIndex): PBufferedPage;
var
  PageNum: Cardinal;
begin
  if PageIdx >= PageCount then
    raise EFileError.Create('Page Index is beyond end of file');
  Result := FBuffer.Search(PageIdx);
  if Result = nil then
    begin
      PageNum := FFAT[PageIdx];
      if PageNum = 0 then    { PageNum might be zero in a BTree file where a node has been freed }
        Result := nil
      else
        begin
          Result := FContainer.Fetch(PageNum);
          Result^.PageIdx := PageIdx;
          Result^.FileID := ID;
          FBuffer.Insert(Result);
        end;
    end
  else
    inc(Result^.RefCount); // ADDED
end;

procedure TFile.Release(var Page: PBufferedPage);
begin
  if Page = nil then Exit;
  if (Page^.RefCount = 1) and (FBuffer.Mode = bmSequential) then
    FBuffer.Remove(Page^.PageIdx)
  else
    if Page^.RefCount > 0 then
      dec(Page^.RefCount);
  Page := nil;
end;

procedure TFile.Truncate(PageIdx: TPageIndex);
var
  I: Integer;
  S: Integer;
begin
  if PageIdx = PageCount then Exit;
  S := PageCount - PageIdx;
  if FContainer.BeginOperation(foDelete) then
    try
      FContainer.DoProgress(Name,0,S);
      FBuffer.Truncate(PageIdx);
      for I := PageCount - 1 downto PageIdx do
        begin
          if I < PageCount then
            Delete(I);
          if I mod 10 = 0 then
            FContainer.DoProgress(Name,S-I,S);
        end;
      // Update the file size
      if Kind = fkBTree then
        SetSize((PageIdx - FFAT.CountZeroPages) * PAGE_SIZE)
      else
        SetSize(PageIdx * PAGE_SIZE);
      FHandles.NotifySizeChanged;
      FContainer.DoProgress(Name,S,S);
    finally
      FContainer.EndOperation;
    end;
end;

function TFile.Insert(PageIdx: TPageIndex): PBufferedPage;
var
  PI: TPageIndex;
begin
  Result := FContainer.Alloc;
  Result^.FileID := ID;
  if (Kind = fkBTree) and FFAT.FindEmptyPage(PI) then
    begin
      Result^.PageIdx := PI;
      FFAT.SetItem(PI,Result^.PageNum);
    end
  else
    begin
      Result^.PageIdx := PageIdx;
      FFAT.Insert(PageIdx,Result^.PageNum);
    end;
  SetSize(Size + PAGE_SIZE);
  FBuffer.Insert(Result);
end;

procedure TFile.Delete(PageIdx: TPageIndex);
var
  PC: Cardinal;
  PageNum: TPageNum;
begin
  PC := PageCount;
  if PageIdx >= PC then
    raise EFileError.Create('Index out of range');
  FHandles.NotifyDelete(PageIdx);
  FBuffer.Remove(PageIdx);
  PageNum := FFAT[PageIdx];
  if PageNum > 0 then
    FContainer.Delete(PageNum);
  if Kind = fkBTree then
    FFAT.SetItem(PageIdx,0)
  else
    FFAT.Delete(PageIdx);
  FFAT.RemoveTrailingZeros;
  if Size > PAGE_SIZE then
    SetSize(Size - PAGE_SIZE)
  else
    SetSize(0);
end;

procedure TFile.Flush;
var
  I: Integer;
begin
  for I := 0 to FHandles.Count - 1 do
    FHandles.FArray[I].FLast := nil;
  FBuffer.Flush;
  FFAT.Flush;
end;

procedure TFile.Delete;
begin
  Truncate(0);
end;

{ TFileBuffer }

constructor TFileBuffer.Create(AFile: TFile);
begin
  FFile := AFile;
  if Assigned(FFile) then                 // This is need for the unit tests
    FContainer := FFile.FContainer;
end;

destructor TFileBuffer.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TFileBuffer.GetPage(Index: Integer): PBufferedPage;
begin
  Result := FArray[Index];
end;

function TFileBuffer.BinSearch(PageIdx: TPageIndex; var Location: Integer): Boolean;
var
  First, Last, Mid: Integer;
  P: Cardinal;
begin
  Result := False;
  Location := 0;
  if FCount > 0 then   { Don't search empty list }
    begin
      First := 0;
      Last := FCount - 1;
      while First <= Last do   { Usual binary search }
        begin
          Mid := (First + Last) div 2;
          P := FArray[Mid]^.PageIdx;
          if PageIdx > P then
            First := Mid + 1
          else
            if PageIdx < P then
              Last := Mid - 1
            else
              begin
                Location := Mid;
                Result := True;
                Exit;
              end;
        end;
      Location := First;
    end;
end;

function TFileBuffer.Search(PageIdx: TPageIndex): PBufferedPage;
var
  I: Integer = 0;
begin
  if BinSearch(PageIdx,I) then
    Result := FArray[I]
  else
    Result := nil;
end;

function TFileBuffer.Insert(Page: PBufferedPage): Boolean;
var
  I: Integer;
  Index: Integer = 0;
begin
  Result := not BinSearch(Page^.PageIdx,Index);
  if Result then
    begin
      if (FCount + 1 >= FSize) or (Index + 1 >= FSize) then
        begin
          FSize := FSize + 1024;
          SetLength(FArray,FSize);
        end;
      inc(FCount);
      for I := FCount - 2 downto Index do
        FArray[I+1] := FArray[I];
      FArray[Index] := Page;
    end;
  Validate;
end;

function TFileBuffer.Remove(PageIdx: TPageIndex): Boolean;
var
  Location: Integer;
begin
  Result := BinSearch(PageIdx,Location);
  if Result then
    begin
      Delete(Location);
      Validate;
    end;
end;

// Write all modified pages to the disk
procedure TFileBuffer.SaveModifiedPages;
var
  I: Integer;
  P: PBufferedPage;
begin
  for I := 0 to FCount - 1 do
    begin
      P := FArray[I];
      Assert(P<>nil);
      if P^.Modified then
        if P^.PageNum < FContainer.PageCount then
          begin
            FContainer.WriteData(P^.PageNum,P^.Data);
            P^.Modified := False;
          end
        else
          Delete(I);
    end;
end;

// Deletes all buffers where RefCount=0
procedure TFileBuffer.DeleteUnusedBuffers;
var
  I: Integer;
  P: PBufferedPage;
begin
  // Delete any buffers where RefCount=0
  for I := FCount - 1 downto 0 do
    begin
      P := FArray[I];
      Assert(P<>nil);
      if P^.RefCount <= 0 then
        Delete(I);
    end;
end;

procedure TFileBuffer.Flush;
begin
  SaveModifiedPages;
  DeleteUnusedBuffers;
  Validate;
end;


procedure TFileBuffer.Truncate(PageIdx: TPageIndex);
var
  I: Integer;
begin
  I := FCount - 1;
  while (I >= 0) and (FArray[I]^.PageIdx >= PageIdx) do
    begin
      Delete(I);
      dec(I);
    end;
  Validate;
end;

// Removes the page at location Index from the buffer and tells the FileContainer to dispose of the page
procedure TFileBuffer.Delete(Index: Integer);
var
  I: Integer;
  P: PBufferedPage;
begin
  // Retrieve the page from the buffer
  P := FArray[Index];
  // Delete the page from the buffer array
  for I := Index to FCount - 2 do
    FArray[I] := FArray[I+1];
  dec(FCount);
  // Dispose of the buffer
  FContainer.Dispose(P);

  Validate;
end;

procedure TFileBuffer.Clear;
var
  I: Integer;
  B: PBufferedPage;
begin
  for I := 0 to FCount - 1 do
    begin
      B := FArray[I];
      if B^.Data <> nil then
        FreeMem(B^.Data);
      FreeMem(B,SizeOf(TBufferedPage));
    end;
  SetLength(FArray,0);
  FSize := 0;
  FCount := 0;
end;

function TFileBuffer.PageNumChanged(OldPageNum, NewPageNum: TPageNum): Boolean;
var
  I: Integer;
  Page: PBufferedPage;
begin
  for I := 0 to FCount - 1 do
    begin
      Page := FArray[I];
      if Page^.PageNum = OldPageNum then
        begin
          Page^.PageNum := NewPageNum;
          Page^.Modified := True;
          Result := True;
          Exit;
        end;
    end;
  Result := False;
  Validate;
end;

procedure TFileBuffer.PageNumSwapped(PageNum1,PageNum2: TPageNum);
var
  I: Integer;
  P: PBufferedPage;
begin
  for I := 0 to FCount - 1 do
    begin
      P := FArray[I];
      if P^.PageNum = PageNum1 then
        P^.PageNum := PageNum2
      else
        if P^.PageNum = PageNum2 then
          P^.PageNum := PageNum1;
    end;
  Validate;
end;

procedure TFileBuffer.CheckOrder;
var
  I: Integer;
  L: Integer;
  P: PBufferedPage;
begin
  L := -MaxInt;
  for I := 0 to FCount - 1 do
    begin
      P := FArray[I];
      if P^.PageIdx <= L then
        raise EFileError.Create('Buffer is out of order');
      L := P^.PageIdx;
    end;
end;

procedure TFileBuffer.CheckRange;
var
  I: Integer;
  P: PBufferedPage;
begin
  for I := 0 to FCount - 1 do
    begin
      P := FArray[I];
      if P^.PageIdx >= FFile.PageCount then
        raise EFileError.Create('PageIdx out of range');
      if P^.PageNum >= FContainer.PageCount then
        raise EFileError.Create('PageNum out of range');
    end;
end;

procedure TFileBuffer.Validate;
begin
  Exit;
  CheckOrder;
  CheckRange;
end;

{ TFileFAT }

constructor TFileFAT.Create(AFile: TFile);
begin
  FFile := AFile;
  FContainer := AFile.FContainer;
  if FFile.ID > 0 then
    begin
      FFilePageCount := (FContainer.FHeader.FileSize[FFile.FID] + PAGE_SIZE - 1) div PAGE_SIZE;
      FFATPageCount  := (FFilePageCount + MAX_FAT_PAGE_ENTRIES - 1) div MAX_FAT_PAGE_ENTRIES;
    end;
end;

destructor TFileFAT.Destroy;
begin
  Release;
  inherited Destroy;
end;

// Releases the memory used by the buffered FAT pages
procedure TFileFAT.Release;
var
  I: Integer;
begin
  for I := 0 to FFATPageCount - 1 do
    FContainer.Release(FPages[I]);
  FLoaded := False;
  FSize := 0;
  SetLength(FPages,FSize);
end;

// Return the number of pages in the FAT that have the value 0.  Used to calculate the actual size of a btree file.
function TFileFAT.CountZeroPages: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FFilePageCount - 1 do
    if Items[I] = 0 then
      inc(Result);
end;

procedure TFileFAT.RemoveTrailingZeros;
var
  X: Integer;
begin
  for X := FFilePageCount - 1 downto 0 do
    begin
      if Items[X] = 0 then
        Delete(X)
      else
        Break;
    end;
end;

function TFileFAT.FindEmptyPage(var PageIdx: TPageIndex): Boolean;
var
  X: Integer;
begin
  Result := False;
  PageIdx := 0;
  if not FLoaded then Fetch;
  for X := 0 to Count - 1 do
    if Items[X] = 0 then
      begin
        Result := True;
        PageIdx := X;
        Exit;
      end;
end;

// Fetches the FAT table from the disk
procedure TFileFAT.Fetch;
var
  PageNum: Integer;
  PageCount: Integer;
  Page: PBufferedPage;
begin
  Assert(not FLoaded);
  if FLoaded then Exit;
  FFilePageCount := (FContainer.FHeader.FileSize[FFile.FID] + PAGE_SIZE - 1) div PAGE_SIZE;
  FFATPageCount  := (FFilePageCount + MAX_FAT_PAGE_ENTRIES - 1) div MAX_FAT_PAGE_ENTRIES;
  FSize := FFATPageCount + 8;
  SetLength(FPages,FSize);
  PageNum := FContainer.FHeader.FATPages[FFile.ID];
  PageCount := 0;
  while PageNum > 0 do
    begin
      Page := FContainer.Fetch(PageNum);
      Assert(Assigned(Page));
      Page^.FileID := FFile.ID;
      PageNum := PFATPage(Page^.Data)^.Next;
      FPages[PageCount] := Page;
      inc(PageCount);
    end;
  if PageCount <> FFATPageCount then
    raise EFileError.CreateFmt('FAT is corrupt for virtual file %s',[FFile.Name]);
  FLoaded := True;
end;

// Retrieves the index'th FAT page
function TFileFAT.GetFATPage(Index: Integer): PBufferedPage;
begin
  Result := FPages[Index];
end;

// Retrieves the Index'th FAT page entry
function TFileFAT.GetItem(Index: TPageIndex): TPageNum;
var
  PageNum: Integer;
  Offset: Integer;
  Page: PBufferedPage;
begin
  if (Index < 0) or (Index >= FFilePageCount) then
    raise EFileError.CreateFmt('Invalid FAT table index %d',[Index]);
  if not FLoaded then Fetch;
  PageNum := Index div MAX_FAT_PAGE_ENTRIES;
  Offset := Index - (PageNum * MAX_FAT_PAGE_ENTRIES);
  Page := FPages[PageNum];
  Assert(Assigned(Page));
  Result := PFATPage(Page^.Data)^.Pages[Offset];
end;

// Sets the FAT entry at Index to AValue
procedure TFileFAT.SetItem(Index: TPageIndex; AValue: TPageNum);
var
  PageNum: Integer;
  Offset: Integer;
  Page: PBufferedPage;
begin
  if (Index < 0) or (Index > FFilePageCount) then
    raise EFileError.CreateFmt('Invalid FAT table index %d',[Index]);
  if not FLoaded then Fetch;
  PageNum := Index div MAX_FAT_PAGE_ENTRIES;
  Offset := Index - (PageNum * MAX_FAT_PAGE_ENTRIES);
  Page := FPages[PageNum];
  PFATPage(Page^.Data)^.Pages[Offset] := AValue;
  Page^.Modified := True;
end;

// Appends PageNum to the FAT
procedure TFileFAT.Push(PageNum: TPageNum);
var
  OldFATPageCount: Integer;
  NewPage, PrevPage: PBufferedPage;
begin
  if not FLoaded then Fetch;
  OldFATPageCount := FFATPageCount;
  inc(FFilePageCount);
  FFATPageCount := (FFilePageCount + MAX_FAT_PAGE_ENTRIES - 1) div MAX_FAT_PAGE_ENTRIES;
  if FFATPageCount > OldFATPageCount then
    begin // Add a FAT page if needed
      if FFATPageCount > FSize then
        begin
          FSize := FSize + 8;
          SetLength(FPages,FSize);
        end;
      NewPage := FContainer.Alloc;
      Assert(Assigned(NewPage));
      NewPage^.FileID := FFile.ID;
      if FFATPageCount = 1 then
        FContainer.FHeader.FATPages[FFile.FID] := NewPage^.PageNum
      else
        begin
          PrevPage := FPages[FFATPageCount - 2];
          Assert(Assigned(PrevPage));
          PFATPage(PrevPage^.Data)^.Next := NewPage^.PageNum;
          PrevPage^.Modified := True;
        end;
      FPages[FFATPageCount - 1] := NewPage;
    end;
  SetItem(FFilePageCount - 1,PageNum);
end;

// Removes the last FAT entry from the FAT
procedure TFileFAT.Pop;
var
  OldFATPageCount: Integer;
  Page: PBufferedPage;
begin
  if not FLoaded then Fetch;
  if FFilePageCount = 0 then
    raise EFileError.Create('FAT table is empty');
  SetItem(FFilePageCount - 1,0);
  OldFATPageCount := FFATPageCount;
  dec(FFilePageCount);
  FFATPageCount := (FFilePageCount + MAX_FAT_PAGE_ENTRIES - 1) div MAX_FAT_PAGE_ENTRIES;
  if FFATPageCount < OldFATPageCount then
    begin
      Page := FPages[OldFATPageCount - 1];
      Assert(Assigned(Page));
      FContainer.Delete(Page);
      Page := FPages[FFATPageCount - 1];
      Assert(Assigned(Page));
      Page^.Modified := True;
      PFATPage(Page^.Data)^.Next := 0;
    end;
end;

// Insert PageNum at position PageIdx
procedure TFileFAT.Insert(PageIdx: TPageIndex; Value: TPageNum);
var
  PageNum: Integer;
  Offset,EndOffset: Integer;
  Next, Page: PBufferedPage;
  I: Integer;
  OverFlow: TPageNum;
begin
  if not FLoaded then Fetch;
  if PageIdx > FFilePageCount then
    begin
      Push(Value);
      Exit;
    end;
  // Create a new page?
  inc(FFilePageCount);
  PageNum := FFilePageCount div MAX_FAT_PAGE_ENTRIES;
  if PageNum > FFATPageCount - 1 then
    begin
      // Expand the array of FAT Pages if necessary
      if FFATPageCount = FSize then
        begin
          FSize := FSize + 8;
          SetLength(FPages,FSize);
        end;
      // Allocate a new page and append to the FAT chain
      if FFATPageCount > 0 then
        begin
          Page := FPages[FFATPageCount - 1];
          Assert(Assigned(Page));
          Page^.Modified := True;
         end;
      Next := FContainer.Alloc;
      Assert(Assigned(Next));
      Next^.FileID := FFile.ID;
      if FFATPageCount > 0 then
        PFATPage(Page^.Data)^.Next := Next^.PageNum
      else
        FContainer.FHeader.FATPages[FFile.ID] := Next^.PageNum;
      FPages[FFATPageCount] := Next;
      inc(FFATPageCount);
    end;
  // Calculate the Offset and Page where the insert should occur
  PageNum := PageIdx div MAX_FAT_PAGE_ENTRIES;
  Offset := PageIdx - (PageNum * MAX_FAT_PAGE_ENTRIES);
  Page := FPages[PageNum];
  Assert(Assigned(Page));
  while Page <> nil do
    begin
      Page^.Modified := True;
      // Insert the entry on this page
      Overflow := PFATPage(Page^.Data)^.Pages[MAX_FAT_PAGE_ENTRIES - 1];
      EndOffset := FFilePageCount - (PageNum * MAX_FAT_PAGE_ENTRIES);
      if EndOffset > MAX_FAT_PAGE_ENTRIES - 1 then
        EndOffset := MAX_FAT_PAGE_ENTRIES - 1;
      for I := EndOffset - 1 downto Offset do
        PFATPage(Page^.Data)^.Pages[I+1] := PFATPage(Page^.Data)^.Pages[I];
      PFATPage(Page^.Data)^.Pages[Offset] := Value;
      // Insert the overflow on the next page
      if PageNum < FFATPageCount - 1 then
        begin
          inc(PageNum);
          Offset := 0;
          Page := FPages[PageNum];
          Assert(Assigned(Page));
          Value := Overflow;
        end
      else
        Page := nil;
    end;
end;

// Deletes the page at PageIdx from the FAT
procedure TFileFAT.Delete(PageIdx: TPageIndex);
var
  PageNum: TPageNum;
  Offset,EndOffset: Integer;
  Next, Page: PBufferedPage;
  OldFATPageCount: Integer;
  I: Integer;
begin
  if not FLoaded then Fetch;
  if PageIdx >= FFilePageCount then
    raise EFileError.Create('Page index out of range');
  // Delete the file page
  OldFATPageCount := FFATPageCount;
  //FContainer.Delete(Items[PageIdx]);
  dec(FFilePageCount);
  // Calculate the location of the FAT entry to delete
  PageNum := PageIdx div MAX_FAT_PAGE_ENTRIES;
  Offset := PageIdx - (PageNum * MAX_FAT_PAGE_ENTRIES);
  Page := FPages[PageNum];
  Assert(Assigned(Page));
  while Page <> nil do
    begin
      Page^.Modified := True;

      // If this is not the last FAT page in the FAT page chain then
      if PageNum < FFATPageCount - 1 then
        begin
          // Delete the entry on this page
          for I := Offset to MAX_FAT_PAGE_ENTRIES - 2 do
            PFATPage(Page^.Data)^.Pages[I] := PFATPage(Page^.Data)^.Pages[I + 1];
          // Copy last element in from next page
          inc(PageNum);
          Next := FPages[PageNum];
          Assert(Assigned(Next));
          PFATPage(Page^.Data)^.Pages[MAX_FAT_PAGE_ENTRIES - 1] := PFATPage(Next^.Data)^.Pages[0];
          // Advance to the next page and delete element 0
          Page := Next;
          Offset := 0;
        end
      else
        begin
          // Delete the entry on this page...don't need to iterate through the entire page as this is the last
          EndOffset := FFilePageCount - (PageNum * MAX_FAT_PAGE_ENTRIES);
          for I := Offset to EndOffset - 1 do
            PFATPage(Page^.Data)^.Pages[I] := PFATPage(Page^.Data)^.Pages[I + 1];
          PFATPage(Page^.Data)^.Pages[EndOffset] := 0;
          Page := nil;
        end;
    end;
  // If this delete caused the last FAT page to no longer be needed then delete the last FAT page and update the chain pointer
  FFATPageCount := (FFilePageCount + MAX_FAT_PAGE_ENTRIES - 1) div MAX_FAT_PAGE_ENTRIES;
  if FFATPageCount < OldFATPageCount then
    begin
      Page := FPages[OldFATPageCount - 1];
      Assert(Assigned(Page));
      FContainer.Delete(Page);
      if FFATPageCount > 0 then
        begin
          Page := FPages[FFATPageCount - 1];
          Assert(Assigned(Page));
          Page^.Modified := True;
          PFATPage(Page^.Data)^.Next := 0;
        end
      else
        FContainer.FHeader.FATPages[FFile.ID] := 0;
    end;
end;

// Saves changes to the FAT to disk
procedure TFileFAT.Flush;
var
  I: Integer;
  P: PBufferedPage;
begin
  Assert(Assigned(FContainer));
  for I := 0 to FFATPageCount - 1 do
    begin
      P := FPages[I];
      Assert(Assigned(P));
      if P^.Modified then
        begin
          FContainer.WriteData(P^.PageNum,P^.Data);
          P^.Modified := False;
        end
    end;
end;

// Responds to a PageNumChanged operation from the Pack operation
function TFileFAT.PageNumChanged(OldPageNum, NewPageNum: TPageNum): Boolean;
var
  I: Integer;
  Page: PBufferedPage;
begin
  Result := False;
  if not FLoaded then Fetch;
  // Update the FAT table for the file
  for I := 0 to FFilePageCount - 1 do
    if Items[I] = OldPageNum then
      begin
        SetItem(I,NewPageNum);
        Result := True;
        Exit;
      end;
  // Update the FAT table entry in the header
  if FContainer.FHeader.FATPages[FFile.FID] = OldPageNum then
    begin
      FContainer.FHeader.FATPages[FFile.FID] := NewPageNum;
      Result := True;
    end;
  // Update the PageNum of the FAT pages and the "Next" pointers in the FAT chain
  for I := 0 to FFATPageCount - 1 do
    begin
      Page := FPages[I];
      Assert(Assigned(Page));
      if Page^.PageNum = OldPageNum then
        begin
          Page^.PageNum := NewPageNum;
          Page^.Modified := True;
          Result := True;
        end;
      if PFATPage(Page^.Data)^.Next = OldPageNum then
        begin
          PFATPage(Page^.Data)^.Next := NewPageNum;
          Page^.Modified := True;
          Result := True;
        end;
    end;
end;

// Responds to a PageNumSwapped message from the Defrag operation
procedure TFileFAT.PageNumSwapped(PageNum1, PageNum2: TPageNum);
var
  I: Integer;
  Page: PBufferedPage;
begin
  if not FLoaded then Fetch;
  // Update the FAT entries for the file
  for I := 0 to FFilePageCount - 1 do
    if Items[I] = PageNum1 then
      SetItem(I,PageNum2)
    else
      if Items[I] = PageNum2 then
        SetItem(I,PageNum1);

  // Update the FAT table entry in the header
  if FContainer.FHeader.FATPages[FFile.FID] = PageNum1 then
    FContainer.FHeader.FATPages[FFile.FID] := PageNum2
  else
    if FContainer.FHeader.FATPages[FFile.FID] = PageNum2 then
      FContainer.FHeader.FATPages[FFile.FID] := PageNum1;

  // Update the PageNum of the FAT pages and the "Next" pointers in the FAT chain
  for I := 0 to FFATPageCount - 1 do
    begin
      Page := FPages[I];
      Assert(Assigned(Page));
      if Page^.PageNum = PageNum1 then
        begin
          Page^.PageNum := PageNum2;
          Page^.Modified := True;
        end
      else
        if Page^.PageNum = PageNum2 then
          begin
            Page^.PageNum := PageNum1;
            Page^.Modified := True;
          end;

      if PFATPage(Page^.Data)^.Next = PageNum1 then
        begin
          PFATPage(Page^.Data)^.Next := PageNum2;
          Page^.Modified := True;
        end
      else
        if PFATPage(Page^.Data)^.Next = PageNum2 then
          begin
            PFATPage(Page^.Data)^.Next := PageNum1;
            Page^.Modified := True;
          end
    end;
end;

{ TFileHandles }

destructor TFileHandles.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TFileHandles.Close;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    FArray[I].Destroy;
  SetLength(FArray,0);
end;

procedure TFileHandles.NotifySizeChanged;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FArray[I].SizeChanged;
end;

procedure TFileHandles.NotifyDelete(PageIdx: TPageIndex);
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FArray[I].DeleteNotification(PageIdx);
end;

procedure TFileHandles.NotifyRootChanged;
var
  I: Integer;
  H: TCustomFileHandle;
begin
  for I := 0 to FCount - 1 do
    begin
      H := FArray[I];
      if H is TBTreeFileHandle then
        TBTreeHack(H as TBTreeFileHandle).RootChanged;
    end;
end;

// Perform a binary search for Handle and set it's location. If found, Result is TRUE, otherwise Result is FALSE and Location contains the insert location where the value should be inserted.
function TFileHandles.BinSearch(Handle: TCustomFileHandle; var Location: Integer): Boolean;
var
  First, Last, Mid: Integer;
  Temp: PtrInt;
begin
  Result := False;
  Location := 0;
  if FCount > 0 then
    begin
      First := 0;
      Last := FCount - 1;
      while First <= Last do
        begin
          Mid := (First + Last) div 2;
          Temp := PtrInt(FArray[Mid]);
          if PtrInt(Handle) > Temp then
            First := Mid + 1
          else
            if PtrInt(Handle) < Temp then
              Last := Mid - 1
            else
              begin
                Location := Mid;
                Result := True;
                Exit;
              end;
        end;
      Location := First;
    end;
end;

procedure TFileHandles.InsertAt(Handle: TCustomFileHandle; Location: Integer);
var
  I: Integer;
begin
  inc(FCount);
  if FCount > FSize then
    begin
      FSize := FSize + 8;
      SetLength(FArray,FSize);
    end;
  for I := FCount - 2 downto Location do
    FArray[I+1] := FArray[I];
  FArray[Location] := Handle;
end;

procedure TFileHandles.Insert(Handle: TCustomFileHandle);
var
  Location: Integer;
begin
  if BinSearch(Handle,Location) then
    raise EFileError.Create('Duplicate file handle')
  else
    InsertAt(Handle,Location);
end;

function TFileHandles.Search(Handle: TCustomFileHandle): Integer;
begin
  if not BinSearch(Handle,Result) then
    Result := -1;
end;

procedure TFileHandles.Delete(Location: Integer);
var
  I: Integer;
begin
  for I := Location to FCount - 2 do
    FArray[I] := FArray[I + 1];
  dec(FCount);
end;

function TFileHandles.GetItem(Index: Integer): TCustomFileHandle;
begin
  Result := FArray[Index];
end;

procedure TFileHandles.Remove(Handle: TCustomFileHandle);
var
  Location: Integer;
begin
  if BinSearch(Handle,Location) then
    Delete(Location)
  else
    raise EFileError.Create('File handle not found');
end;

{ TCustomFileHandle }

constructor TCustomFileHandle.Create(AFile: TFile);
begin
  inherited Create;
  {$IFNDEF DEBUG}Log.Filter := [mtInfo,mtError];{$ENDIF}
  FContainer := AFile.FContainer;
  FFile      := AFile;
  FFile.FHandles.Insert(Self);
end;

destructor TCustomFileHandle.Destroy;
begin
  Flush;
  FFile.FHandles.Remove(Self);
  inherited Destroy;
end;

function TCustomFileHandle.GetID: Byte;
begin
  Result := FFile.ID;
end;

function TCustomFileHandle.GetPageCount: Cardinal;
begin
  Result := FFile.PageCount
end;

function TCustomFileHandle.GetSize: Cardinal;
begin
  Result := FFile.Size;
end;

procedure TCustomFileHandle.SetSize(const AValue: Cardinal);
begin
  FFile.SetSize(AValue);
end;

procedure TCustomFileHandle.Flush;
begin
  FFile.Flush;
end;

procedure TCustomFileHandle.SizeChanged;
begin
  // Override in descendants
end;

procedure TCustomFileHandle.ReleaseBuffer;
begin
  if FLast <> nil then
    begin
      _File.Release(FLast);
      FLast := nil;
    end;
end;

procedure TCustomFileHandle.DeleteNotification(PageIdx: TPageIndex);
begin
  if (FLast <> nil) and (FLast^.PageIdx = PageIdx) then
    begin
      FLast^.Modified := False;
      FLast^.RefCount := 1;
      FFile.Release(FLast);
      FLast := nil;
    end;
end;

{ TPageFileHandle }

constructor TPageFileHandle.Create(AFile: TFile);
begin
  inherited Create(AFile);
  FFile.Buffer.FMode := bmRandomAccess;
end;

function TPageFileHandle.Fetch(PageIdx: TPageIndex): PBufferedPage;
begin
  Result := FFile.Fetch(PageIdx);
  if FFile.Buffer.FMode = bmSequential then
    if FLast = nil then
      FLast := Result
    else
      if Result <> FLast then
        begin
          Release(FLast);
          FLast := Result;
        end;
end;

procedure TPageFileHandle.Release(var Page: PBufferedPage);
begin
  FFile.Release(Page);
end;

function TPageFileHandle.Alloc: PBufferedPage;
begin
  Result := FFile.Alloc;
  if FFile.Buffer.Mode = bmSequential then
    begin
      if FLast <> nil then
        FFile.Release(FLast);
      FLast := Result;
    end;
end;

procedure TPageFileHandle.Truncate(PageIdx: TPageIndex);
begin
  if (FLast <> nil) and (FLast^.PageIdx >= PageIdx) then
    begin
      FLast^.Modified := False;
      FFile.Release(FLast);
    end;
  Assert(FLast = nil);
  FFile.Truncate(PageIdx);
end;

function TPageFileHandle.Insert(PageIdx: TPageIndex): PBufferedPage;
begin
  Result := FFile.Insert(PageIdx);
end;

procedure TPageFileHandle.Delete(PageIdx: TPageIndex);
begin
  FFile.Delete(PageIdx);
end;

procedure TPageFileHandle.Delete(var Page: PBufferedPage);
begin
  if Page <> nil then
    begin
      Page^.Modified := False;
      Delete(Page^.PageIdx);
      Page := nil;
    end;
end;

{ TStreamFileHandle }

procedure TStreamFileHandle.SizeChanged;
var
  NewSize: Cardinal;
begin
  inherited;
  NewSize := _File.Size;
  if NewSize < FPosition then
    FPosition := NewSize;
end;

function TStreamFileHandle.Read(var Buffer; Count: Longint): Longint;
var
  Page: PBufferedPage;
  PageIdx: TPageIndex;
  Offset: PtrInt;
  AvailableSize: Word;
begin
  // Calculate the page and offset for the read
  PageIdx := FPosition div PAGE_SIZE;
  Offset := FPosition mod PAGE_SIZE;
  // Calculate how much data should be read
  AvailableSize := PAGE_SIZE - Offset;
  if AvailableSize > Count then
    Result := Count
  else
    Result := AvailableSize;
  // Ensure the read doesn't proceed beyond EOF
  if FPosition + Result > Size then
    Result := Size - FPosition;
  if Result = 0 then
    raise EFileError.Create('End of file');

  // Retrieve the page
  if FLast = nil then
    begin
      Page := FFile.Fetch(PageIdx);
      FLast := Page;
    end
  else
    if FLast^.PageIdx = PageIdx then
      Page := FLast
    else
      begin
        FFile.Release(FLast);
        Page := FFile.Fetch(PageIdx);
        FLast := Page;
      end;

  // Move the data and increment position pointer
  Move((Page^.Data + Offset)^,Buffer,Result);
  FPosition := FPosition + Result;

  // If there is more data to read then recursively call read
  if (FPosition < Size) and (Result < Count) then
    Result := Result + Read(TByteArray(Buffer)[Result],Count - Result);
end;

function TStreamFileHandle.Read(Length: Integer): String;
//var
  //P: PChar;
begin
  SetLength(Result,Length);
  Read(Result,Length);
{  P := StrAlloc(Length+1);
  try
    Read(P,Length);
    P[Length] := #0;
    Result := StrPas(P);
  finally
    StrDispose(P);
  end;   }
end;

function TStreamFileHandle.Write(const Buffer; Count: Longint): Longint;
var
  PageIdx: TPageIndex;      // The page where the data segment to write is located
  Offset: Word;             // The offset from the start of this page where the write should start from
  AvailableSize: Integer;   // The number of bytes that can be written to the current Page.  (May be less than Size).
  EndingPosition: Cardinal; // The file position after the write is completed
  Page: PBufferedPage;      // The buffered page to be written to disk
begin
  // Calculate the page and offset for the write
  PageIdx := FPosition div PAGE_SIZE;
  Offset  := FPosition mod PAGE_SIZE;
  // Calculate how much data should be written to the current page
  AvailableSize := PAGE_SIZE - Offset;     // Number of bytes that can be written to the current page
  if AvailableSize > Count then
    Result := Count
  else
    Result := AvailableSize;

  if FLast = nil then
    Page := nil
  else
    if FLast^.PageIdx = PageIdx then
      Page := FLast
    else
      begin
        Page := nil;
        if FLast <> nil then
          FFile.Release(FLast);
      end;

  if Page = nil then
    begin
      if PageIdx >= FFile.PageCount then
        Page := FFile.Alloc
      else
        Page := FFile.Fetch(PageIdx);
      FLast := Page;
    end;

  // If the write will increase the file size then adjust the FileSize field in the header page
  EndingPosition := (PageIdx * PAGE_SIZE) + Offset + Result;
  if EndingPosition > Size then
    SetSize(EndingPosition);

  // Move the data into the page and set the state of the page buffer to modified
  Move(Buffer,(Page^.Data + Offset)^,Result);
  Page^.Modified := True;
  FPosition := EndingPosition;

  // If there is more data to be written then recursively call Write
  if Result < Count then
    Result := Result + Write(TByteArray(Buffer)[Result],Count - Result);
end;

function TStreamFileHandle.Write(Str: String): LongInt;
begin
  Result := Write(PAnsiChar(Str)^,Length(Str));
end;

procedure TStreamFileHandle.Seek(Position: Cardinal);
begin
  if FPosition > Size then
    FPosition := Size
  else
    FPosition := Position;
end;

procedure TStreamFileHandle.Truncate(Position: Cardinal);
var
  PageIdx: TPageIndex;
begin
  PageIdx := Position div PAGE_SIZE;
  if Position mod PAGE_SIZE > 0 then
    inc(PageIdx);
  if FLast <> nil then
    FFile.Release(FLast);
  FFile.Truncate(PageIdx);
  SetSize(Position);
  FPosition := Position;
end;

{ TFreePages }

// Performs a binary search of the free page list
function TFreePages.BinSearch(Key: TPageNum; var Location: Integer): Boolean;
var        // Sorted in reverse order
  First, Last, Mid: Integer;
  PageNum: TPageNum;
begin
  Result := False;
  Location := 1;
  if Count > 0 then   { Don't search empty list }
    begin
      First := 1;
      Last := Count;
      while First <= Last do   { Usual binary search }
        begin
          Mid := (First + Last) div 2;
          PageNum := Items[Mid];
          if Key > PageNum then
            Last := Mid - 1
          else
            if Key < PageNum then
              First := Mid + 1
            else
              begin
                Location := Mid;
                Result := True;
                Exit;
              end;
        end;
      Result := False;
      Location := First;
      //Assert(Key>Items[Location]);
    end;
end;

// Returns TRUE if the free page list is in order
function TFreePages.CheckOrder: Boolean;
var
  X: Integer;
  L: TPageNum;
begin
  L := MaxInt;
  for X := 1 to Count do
    begin
      if FContainer.FHeader.FreePages[X] > L then
        begin
          Result := False;
          Exit;
        end
      else
        L := FContainer.FHeader.FreePages[X];
    end;
  Result := True;
end;

// Inserts PageNum at Location
procedure TFreePages.InsertAt(PageNum: TPageNum; Location: Integer);
var
  I: Integer;
begin
  Assert((Location > 0) and (Location <= MAX_FREE_PAGES),'TFreePages.Insert Location is out of range');
  Assert(Count < MAX_FREE_PAGES);
  for I := Count downto Location do
    FContainer.FHeader.FreePages[I+1] := Items[I];
  FContainer.FHeader.FreePages[Location] := PageNum;
  inc(FContainer.FHeader.FreePageCount);
end;

// Deletes the PageNum at Location
procedure TFreePages.DeleteAt(Location: Integer);
var
  I: Integer;
begin
  Assert((Location > 0) and (Location <= MAX_FREE_PAGES),'TFreePages.Delete Location is out of range');
  for I := Location + 1 to Count do
    FContainer.FHeader.FreePages[I - 1] := Items[I];
  if Count > 0 then
    dec(FContainer.FHeader.FreePageCount);
end;

// Inserts PageNum in the free page list
procedure TFreePages.Insert(PageNum: TPageNum);
var
  Location: Integer;
begin
  if Count >= MAX_FREE_PAGES - 8 then  { If the last page is being deleted, pack will not be able to get any free pages.  TF don't let the free page list get close to the limit }
    FContainer.Pack;
  Assert(Count < MAX_FREE_PAGES,'Failed to add page number to the free page list');
  if BinSearch(PageNum,Location) then
    Assert(False,'Duplicate free page entry')
  else
    InsertAt(PageNum,Location);
end;

// Removes PageNum from the free page list
function TFreePages.Remove(PageNum: TPageNum): Boolean;
var
  Location: Integer;
begin
  Result := BinSearch(PageNum,Location);
  if Result then
    DeleteAt(Location);
end;

// Deletes any PageNum that is >= PageNum
procedure TFreePages.Truncate(PageNum: TPageNum);
var
  I: Integer;
begin
  if PageNum = 0 then
    FContainer.FHeader.FreePageCount := 0
  else
    for I := 1 to Count do
      if Items[I] >= PageNum then
        DeleteAt(I)
      else
        Break;
end;

// Retrieves the lowest PageNum from the free page list
function TFreePages.GetLowest: TPageNum;
begin
  if Count > 0 then
    begin
      Result := Items[Count];
      dec(FContainer.FHeader.FreePageCount);
      if Result > FContainer.PageCount then
        begin
          FContainer.FHeader.FreePageCount := 0;
          Result := 0;
        end;
    end
  else
    Result := 0;
end;

// Responds to a PageNumChanged message generated by the Pack routine
function TFreePages.PageNumChanged(OldPageNum, NewPageNum: TPageNum): Boolean;
var
  Location: Integer;
begin
  Result := BinSearch(OldPageNum,Location);
  if Result then
    begin
      DeleteAt(Location);
      Insert(NewPageNum);
    end;
end;

// Responds to a PageNumSwapped message generated by the Defrag routine
procedure TFreePages.PageNumSwapped(PageNum1, PageNum2: TPageNum);
var
  Loc1, Loc2: Integer;
  Res1, Res2: Boolean;
begin
  Res1 := BinSearch(PageNum1,Loc1);
  if Res1 then
    DeleteAt(Loc1);
  Res2 := BinSearch(PageNum2,Loc2);
  if Res2 then
    DeleteAt(Loc2);
  if Res1 then
    Insert(PageNum2);
  if Res2 then
    Insert(PageNum1);
end;

// Gets the number of free pages in the list
function TFreePages.GetCount: Integer;
begin
  Result := FContainer.FHeader.FreePageCount;
end;

// Sets the number of free pages in the list
{procedure TFreePages.SetCount(AValue: Integer);
begin
  Assert((AValue >= 0) and (AValue <= MAX_FREE_PAGES),'Free page count out of range');
  FContainer.FHeader.FreePageCount := AValue;
end;}

// Gets the free page entry at index
function TFreePages.GetItem(Index: Integer): TPageNum;
begin
  Assert((Index > 0) and (Index <= MAX_FREE_PAGES),'Free page index out of range');
  Result := FContainer.FHeader.FreePages[Index];
end;

// Sets the free page entry at Index to Value
{procedure TFreePages.SetItem(Index: Integer; AValue: TPageNum);
begin
  Assert((Index > 0) and (Index <= MAX_FREE_PAGES),'Free page index out of range');
  FContainer.FHeader.FreePages[Index] := AValue;
end;}

{ TFileMetadata }

// Retrieve the index'th metadata page
function TFileMetadata.GetPage(Index: Integer): TPageNum;
begin
  Result := FContainer.FHeader.MetaPages[Index];
end;

// Returns the size of the metadata stream
function TFileMetadata.GetSize: Cardinal;
begin
  Result := FContainer.FHeader.MetaSize;
end;

// Return the number of metadata pages
function TFileMetadata.GetPageCount: Integer;
begin
  Result := (FContainer.FHeader.MetaSize + PAGE_SIZE - 1) div PAGE_SIZE;
end;

// Responds to a PageNumChanged message generated by the Pack routine
function TFileMetadata.PageNumChanged(OldPageNum, NewPageNum: TPageNum): Boolean;
var
  I: Word;
begin
  for I := 1 to PageCount do
    if Pages[I] = OldPageNum then
      begin
        FContainer.FHeader.MetaPages[I] := NewPageNum;
        Result := True;
        Exit;
      end;
  Result := False;
end;

// Responds to a PageNumSwapped message generated by the Defrag routine
procedure TFileMetadata.PageNumSwapped(PageNum1,PageNum2: TPageNum);
var
  PageNum: TPageNum;
  I: Integer;
begin
  for I := 1 to PageCount do
    begin
      PageNum := Pages[I];
      if PageNum = PageNum1 then
        FContainer.FHeader.MetaPages[I] := PageNum2
      else
        if PageNum = PageNum2 then
          FContainer.FHeader.MetaPages[I] := PageNum1;
    end;
end;

// Saves file metadata from Stream into the Container
procedure TFileMetadata.SaveFromStream(Stream: TStream);
var
  Page: PBufferedPage;
  RemainingSize: Integer;
  BlockSize: Integer;
  PageIdx: TPageIndex;
  PageNum: TPageNum;
  I: Integer;
begin
  Stream.Position := 0;
  RemainingSize := Stream.Size;
  PageIdx := 1;
  while RemainingSize > 0 do
    begin
      PageNum := Pages[PageIdx];
      if PageNum = 0 then
        begin
          Page := FContainer.Alloc;
          PageNum := Page^.PageNum;
          FContainer.FHeader.MetaPages[PageIdx] := PageNum;
        end
      else
        Page := FContainer.Fetch(PageNum);
      if RemainingSize > PAGE_SIZE then
        BlockSize := PAGE_SIZE
      else
        BlockSize := RemainingSize;
      Stream.Read(Page^.Data^,BlockSize);
      Page^.Modified := True;
      FContainer.Release(Page);
      RemainingSize := RemainingSize - BlockSize;
      inc(PageIdx);
    end;
  for I := PageIdx to MAX_META_PAGES do
    begin
      PageNum := Pages[I];
      if PageNum > 0 then
        FContainer.Delete(PageNum);
      FContainer.FHeader.MetaPages[I] := 0;
    end;
  FContainer.FHeader.MetaSize := Stream.Size;
end;

// Loads file metadata from the container into Stream
procedure TFileMetadata.LoadToStream(Stream: TStream);
var
  Page: PBufferedPage;
  RemainingSize: Integer;
  BlockSize: Integer;
  PageIdx: Integer;
begin
  RemainingSize := FContainer.FHeader.MetaSize;
  Stream.Size := RemainingSize;
  PageIdx := 1;
  while RemainingSize > 0 do
    begin
      Page := FContainer.Fetch(FContainer.FHeader.MetaPages[PageIdx]);
      if RemainingSize > PAGE_SIZE then
        BlockSize := PAGE_SIZE
      else
        BlockSize := RemainingSize;
      Stream.Write(Page^.Data^,BlockSize);
      FContainer.Release(Page);
      RemainingSize := RemainingSize - BlockSize;
      inc(PageIdx);
    end;
end;

constructor TFileMetadata.Create;
begin
  {$IFNDEF DEBUG}Log.Filter := [mtInfo,mtError];{$ENDIF}
end;

{ TFileContainer }

constructor TFileContainer.Create;
begin
  inherited Create;
  {$IFNDEF DEBUG}Log.Filter := [mtInfo,mtError];{$ENDIF}
  FFreePages := TFreePages.Create;
  FFreePages.FContainer := Self;
  FFiles := TFiles.Create;
  FFiles.FContainer := Self;
  FMetadata := TFileMetadata.Create;
  FMetadata.FContainer := Self;
  FPageMap := TPageMap.Create(Self);
end;

destructor TFileContainer.Destroy;
begin
  Active := False;
  FPageMap.Free;
  FMetadata.Destroy;
  FFreePages.Destroy;
  FFiles.Destroy;
  inherited Destroy;
end;

// OPEN & CLOSE

// Opens or closes the file
procedure TFileContainer.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  if AValue then Open else Close;
end;

// Opens the file container, prompts for a password, sets up encryption, and retrieves the metadata stream.
// If the user cannot provide a valid password then Open may not complete and Active will not be set to TRUE.
procedure TFileContainer.Open;
var
  {$IFDEF Unix}
  Pos: Int64;
  {$ENDIF}
  {$IFDEF Windows}
  ShareMode: DWORD;
  {$ENDIF}
begin
  {$IFDEF Unix}
  FHandle := FpOpen(PChar(FFilename),O_RDWR or O_CREAT,S_IWUSR or S_IRUSR or S_IRGRP or S_IWGRP);
  if FHandle = -1 then
    raise EFileError.Create(GetErrorMessage(ErrNo));
  FActive := True;
  Pos := FpLSeek(FHandle,0,SEEK_END);
  if Pos = -1 then
    raise EFileError.Create(GetErrorMessage(ErrNo));
  FPageCount := Pos div PAGE_SIZE;
  {$ENDIF}
  {$IFDEF Windows}
  ShareMode := FILE_SHARE_READ or FILE_SHARE_WRITE;
  FHandle := Windows.CreateFile(PChar(FFilename),GENERIC_READ or GENERIC_WRITE,ShareMode,nil,OPEN_ALWAYS,FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS,0);
  if FHandle = INVALID_HANDLE_VALUE then
    raise EFileError.Create(GetErrorMessage(GetLastError));
  FActive := True;
  FPageCount := SetFilePointer(FHandle,0,nil,FILE_END) div PAGE_SIZE;
  if FPageCount = $FFFFFFFF then
    raise EFileError.Create(GetErrorMessage(GetLastError));
  {$ENDIF}
  if FActive then
    Log.Send(mtInfo,'Opened file container %s',[FFilename]);
  if FPageCount < 1 then
    begin
      CreateHeader;
      ClearMetadata;
    end
  else
    begin
      ReadHeader;
      if Login then
        begin
          Log.Send(mtInfo,'Authentication successful.');
          if Assigned(FCipher) then
            FreeAndNil(FCipher);
          if FHeader.Cipher <> caNone then
            begin
              //Log.Send(mtDebug,'Begin decrypt of header');
              InitCipherKey;
              FCipher.InitMode(cmCFB,@FHeader.Digest);
              FCipher.Decrypt(@FHeader.FreePageCount,PAGE_SIZE - 64);
              //Log.Send(mtDebug,'End decrypt of header');
            end;
          ClearMetadata;
          LoadMetadata;
        end
      else
        begin
          Log.Send(mtError,'Authentication failed.');
          FActive := False;
          FPageCount := 0;
          {$IFDEF Unix}
          if FpClose(FHandle) <> 0 then
            raise EFileError.Create(GetErrorMessage(ErrNo));
          {$ENDIF}
          {$IFDEF Windows}
          if not CloseHandle(FHandle) then
            raise EFileError.Create(GetErrorMessage(GetLastError));
          {$ENDIF}
          FillChar(FHeader,SizeOf(TFileHeader),0);
          Exit;
        end;
    end;
end;

// Flush and Close the file container.
procedure TFileContainer.Close;
begin
  Flush;
  FFiles.Clear;
  FActive := False;
  FPageCount := 0;
  {$IFDEF Unix}
  if FpClose(FHandle) <> 0 then
    raise EFileError.Create(GetErrorMessage(ErrNo));
  {$ENDIF}
  {$IFDEF Windows}
  if not CloseHandle(FHandle) then
    raise EFileError.Create(GetErrorMessage(GetLastError));
  {$ENDIF}
  Log.Send(mtDebug,'File container %s closed.',[FFilename]);
  FillChar(FHeader,SizeOf(TFileHeader),0);
end;

// HEADER

// Writes a new header page to the container
procedure TFileContainer.CreateHeader;
begin
  FFiles.Clear;
  Assert(SizeOf(TFileHeader) = PAGE_SIZE,'Header size ' + IntToStr(SizeOf(TFileHeader)) + ' is invalid');
  FillChar(FHeader,PAGE_SIZE,0);
  FHeader.Version := DEFAULT_VERSION;
  WriteData(0,@FHeader);
end;

// Read the header page from the container.  Validate the file version.  Does not decrypt header if it is encrypted.
procedure TFileContainer.ReadHeader;
begin
  Assert(SizeOf(TFileHeader) = PAGE_SIZE);
  ReadData(0,@FHeader);
  if (FHeader.Version <> DEFAULT_VERSION) then
    raise EFileError.Create('Wrong file version');
end;

// Writes the header page to the container.  Encrypts the encryptable portion of the header if encryption is enabled.
procedure TFileContainer.WriteHeader;
var
  L: TFileHeader;
begin
  if Encrypted then
    begin
      //Log.Send(mtDebug,'WriteHeader Key='+BufferToHex(@FKey,SizeOf(FKey)));
      //Log.Send(mtDebug,'WriteHeader Digest='+BufferToHex(@FHeader.Digest,SizeOf(FHeader.Digest)));
      L := FHeader;
      FCipher.InitMode(cmCFB,@FHeader.Digest);
      FCipher.Encrypt(@L.FreePageCount,PAGE_SIZE - 64);
      WriteData(0,@L);
    end
  else
    WriteData(0,@FHeader);
end;

// PROTECTED FILE IO and BUFFER ROUTINES

// Reads data from the container
procedure TFileContainer.ReadData(PageNum: TPageNum; Data: Pointer);
var
  {$IFDEF Unix}S: Integer;{$ENDIF}
  {$IFDEF Windows}S: DWORD;{$ENDIF}
begin
  if PageNum >= PageCount then
    raise EFileError.Create('Attempted to read from beyond end of file');
  {$IFDEF Unix}
  if FpLSeek(FHandle,PageNum*PAGE_SIZE,SEEK_SET) = -1 then
    raise EFileError.Create(GetErrorMessage(ErrNo));
  S := FpRead(FHandle,Data,PAGE_SIZE);
  if S = 0 then
    raise EFileError.Create('End of file')
  else
    if S = -1 then
      raise EFileError.Create(GetErrorMessage(ErrNo));
  {$ENDIF}
  {$IFDEF Windows}
  if SetFilePointer(FHandle,PageNum*PAGE_SIZE,nil,FILE_BEGIN) = $FFFFFFFF then
    raise EFileError.Create('Invalid file position');
  if not ReadFile(FHandle,Data^,PAGE_SIZE,S,nil) then
    raise EFileError.Create(GetErrorMessage(GetLastError));
  {$ENDIF}
  if Encrypted and (PageNum > 0) and not FEncrypting then
    DecryptPage(PageNum,Data);
  if S <> PAGE_SIZE then
    raise EFileError.Create('Could not read full page');
end;

// Writes data to the container file.
procedure TFileContainer.WriteData(PageNum: TPageNum; Data: Pointer);
var
  Data2: Pointer;
  {$IFDEF Unix}S: Integer;{$ENDIF}
  {$IFDEF Windows}S: DWORD;{$ENDIF}
begin
  Assert(PageNum <= PageCount);
  if Encrypted and (PageNum > 0) and not FEncrypting then
    begin
      Data2 := GetMem(PAGE_SIZE);
      Move(Data^,Data2^,PAGE_SIZE);
      EncryptPage(PageNum,Data2);
    end
  else
    Data2 := Data;

  {$IFDEF Unix}
  if FpLSeek(FHandle,PageNum*PAGE_SIZE,SEEK_SET) = -1 then
    raise EFileError.Create(GetErrorMessage(ErrNo));
  S := FpWrite(FHandle,Data2^,PAGE_SIZE);
  if S = 0 then
    raise EFileError.Create('End of file')
  else
    if S = -1 then
      raise EFileError.Create(GetErrorMessage(ErrNo));
  {$ENDIF}

  {$IFDEF Windows}
  if SetFilePointer(FHandle,PageNum*PAGE_SIZE,nil,FILE_BEGIN) = $FFFFFFFF then
    raise EFileError.Create('Invalid file position');
  if not WriteFile(FHandle,Data2^,PAGE_SIZE,S,nil) then
    raise EFileError.Create(GetErrorMessage(GetLastError));
  {$ENDIF}

  if Encrypted and (PageNum > 0) and (Data2 <> nil) and not FEncrypting then
    FreeMem(Data2);

  if S <> PAGE_SIZE then
    raise EFileError.Create('Could not write full page.  File is probably corrupted.');

  if PageNum >= PageCount then
    inc(FPageCount);
end;

 // Allocates a new page in the container.  Tries to obtain the new page from the free page list, otherwise a new page is appended to the container.
function TFileContainer.Alloc: PBufferedPage;
begin
  // Allocate memory for a new buffer page and initialize it to zero
  Result := GetMem(SizeOf(TBufferedPage));
  FillChar(Result^,SizeOf(TBufferedPage),0);
  Result^.Data := GetMem(PAGE_SIZE);
  FillChar(Result^.Data^,PAGE_SIZE,0);
  // Assign a pagenum to the new record
  if FFreePages.Count > 0 then
    Result^.PageNum := FFreePages.GetLowest     // Try free page table first
  else
    begin
      Result^.PageNum := FPageCount;      // Otherwise add a new page to the file
      inc(FPageCount);
    end;
  // By default the new page is modified and it's RefCount is 1
  Result^.Modified := True;
  Result^.RefCount := 1;
end;

// Fetches PageNum from disk.  All values of a PBufferedPage are initialized to zero before the page is loaded.
function TFileContainer.Fetch(PageNum: TPageNum): PBufferedPage;
begin
  if PageNum > PageCount then
    raise EFileError.Create('Attempted to fetch a page beyond the end of file');
  GetMem(Result,SizeOf(TBufferedPage));
  FillChar(Result^,SizeOf(TBufferedPage),0);
  Result^.PageNum := PageNum;
  Result^.Data := GetMem(PAGE_SIZE);
  ReadData(PageNum,Result^.Data);
  Result^.RefCount := 1;
end;

// Decrements Page's reference count and diposes it when it's reference count becomes zero.
// Sets Page to nil on return.  If Page is modified the changes will be written to disk before it is disposed.
procedure TFileContainer.Release(var Page: PBufferedPage);
begin
  if (Page <> nil) then
    begin
      dec(Page^.RefCount);
      if Page^.RefCount <= 0 then
        Dispose(Page)  { This sets Page to nil }
      else
        Page := nil;
    end;
end;

// Writes a modified page to the disk before disposing of it's memory.  Sets Page to nil. Works regardless of RefCount.
procedure TFileContainer.Dispose(var Page: PBufferedPage);
begin
  if Page = nil then Exit;
  if Page^.Modified then
    WriteData(Page^.PageNum,Page^.Data);
    // Free any tree node assigned to the page
//  if Assigned(Page^.Node) then
//    Page^.Node.Destroy;
  FreeMem(Page^.Data,PAGE_SIZE);
  FreeMem(Page,SizeOf(TBufferedPage));
  Page := nil;
end;

// Deletes Page from the container, discarding any changes to Page, releasing it's memory,
// and setting Page to nil.
procedure TFileContainer.Delete(var Page: PBufferedPage);
var
  PageNum: TPageNum;
begin
  if Page = nil then
    raise EFileError.Create('Attempted to delete a nil page buffer');
  PageNum := Page^.PageNum;
  Page^.Modified := False;
  Release(Page);
  Delete(PageNum);
end;

// Deletes the page PageNum from the container.
// If AutoPack is TRUE then the last page of the file is swapped in to replace the deleted page and all page numbers are updated.
// If AutoPack is FALSE then the deleted page is added to the free page list.  If the free page list is near full then Pack is called.
procedure TFileContainer.Delete(PageNum: TPageNum);
begin
  FDeletingPage := PageNum; { Prevents PageNum from being used in a Pack operation }
  try
    if PageNum = PageCount - 1 then
      begin
        Log.Send(mtDebug,'Delete(%d) handled by Truncate()',[PageNum]);
        Truncate(PageNum);
      end
    else
      if AutoPack then
        Replace(PageCount-1,PageNum)
      else
        FreePages.Insert(PageNum);
  finally
    FDeletingPage := 0;
  end;
end;

// Truncates the file container at PageNum
procedure TFileContainer.Truncate(PageNum: TPageNum);
begin
  FreePages.Truncate(PageNum);
  {$IFDEF Unix}
  if FPfTruncate(FHandle,PageNum * PAGE_SIZE) = -1 then
    raise EFileError.Create(GetErrorMessage(ErrNo));
  {$ENDIF}
  {$IFDEF Windows}
  if SetFilePointer(FHandle,PageNum*PAGE_SIZE,nil,FILE_BEGIN) = $FFFFFFFF then
    raise EFileError.Create('Invalid file position: '+GetErrorMessage(GetLastError));
  if not SetEndOfFile(FHandle) then
    raise EFileError.Create(GetErrorMessage(GetLastError));
  {$ENDIF}
  FPageCount := PageNum;
end;

// Ensures all buffered information has been written to disk
procedure TFileContainer.Flush;
begin
  FFiles.Flush;
  SaveMetadata;
  DeleteTrailingFreePages;
  WriteHeader;
  {$IFDEF Windows}
  FlushFileBuffers(FHandle);
  {$ENDIF}
end;

// FILESYSTEM ROUTINES

// Returns TRUE if Filename exists, FALSE otherwise
function TFileContainer.FileExists(Filename: String): Boolean;
begin
  Result := FFiles.Find(Filename) <> nil;
end;

// Opens the file Filename creating it if it doesn't exist.  The type of handle returned depends on
// the value of Kind.  Kind is stored for future use in OpenFile
function TFileContainer.CreateFile(Filename: String; Kind: TFileKind = fkStream): TCustomFileHandle;
var
  F: TFile;
begin
  if FileExists(Filename) then
    Result := OpenFile(Filename,Kind)
  else
    begin
      F := TFile.Create(Self,Filename,FFiles.GetID,Kind);
      Result := F.CreateHandle(Kind);
      Log.Send(mtInfo,'Created virtual file %s',[Filename]);
    end;
end;

// Opens the virtual file Filename and returns a TCustomFileHandle descendant according to Kind.
// This allows the caller to temporarily open a file using a different handle
// type than the one normally used to access the file, such as for an import/export operation
// Throws an exception if the file was not found.
function TFileContainer.OpenFile(Filename: String; Kind: TFileKind): TCustomFileHandle;
var
  F: TFile;
begin
  F := FFiles.Find(Filename);
  if F = nil then
    raise EFileError.CreateFmt('File %s not found',[Filename]);
  Result := F.CreateHandle(Kind);
end;

// Opens the virtual file Filename and returns a handle based on the TFileKind
// used when the file was created.  Throws an exception if the file was not found
function TFileContainer.OpenFile(Filename: String): TCustomFileHandle;
var
  F: TFile;
begin
  F := FFiles.Find(Filename);
  if F = nil then
    raise EFileError.CreateFmt('File %s not found',[Filename]);
  Result := F.CreateHandle(F.Kind);
end;

// Renames a virtual file.  Throws an exception if OldName not found or if NewName already exists.
procedure TFileContainer.RenameFile(OldName, NewName: String);
begin
  FFiles.Rename(OldName,NewName);
  Log.Send(mtInfo,'Renamed virtual file %s to %s',[OldName,NewName]);
end;

// Deletes the virtual file Filename.
// Throws an exception if the file has open file handles.
// Returns TRUE if the the file was deleted, FALSE otherwise.
function TFileContainer.DeleteFile(Filename: String): Boolean;
var
  F: TFile;
begin
  F := FFiles.Find(Filename);
  Result := F <> nil;
  if Result then
    begin
      Flush;
      Result := F.FHandles.Count = 0;
      if not Result then
        raise EFileError.CreateFmt('Could not delete virtual file %s because it has open file handles.',[Filename]);
      F.Truncate(0);
      F.Flush;
      Files.Remove(F);
      Log.Send(mtInfo,'Deleted virtual file %s',[Filename]);
    end;
end;

// IMPORT/EXPORT

// Import Stream and save as Filename
procedure TFileContainer.ImportFile(Stream: TStream; Filename: String);
var
  S: TStreamFileHandle;
  B: Pointer;
  L: Int64;
  F: TFile;
begin
  if not Assigned(Stream) then
    raise EFileError.Create('Stream not assigned');
  Active := True;
  if BeginOperation(foImport) then
    try
      F := Files.Find(Filename);
      if F = nil then
        F := TFile.Create(Self,Filename,FFiles.GetID,fkStream);
      S := F.CreateHandle(fkStream) as TStreamFileHandle;
      try
        Stream.Position := 0;
        FOperation := foNone;
        S.Truncate(0);
        FOperation := foImport;
        S.Flush;
        DoProgress(Filename,0,Stream.Size);
        B := GetMem(IMPORT_EXPORT_CHUNK_SIZE);
        try
          while Stream.Size - Stream.Position >= IMPORT_EXPORT_CHUNK_SIZE do
            begin
              Stream.Read(B^,IMPORT_EXPORT_CHUNK_SIZE);
              S.Write(B^,IMPORT_EXPORT_CHUNK_SIZE);
              S.Flush;
              DoProgress(Filename,Stream.Position,Stream.Size);
            end;
          L := Stream.Size - Stream.Position;
          Stream.Read(B^,L);
          S.Write(B^,L);
          DoProgress(Filename,Stream.Size,Stream.Size);
        finally
          FreeMem(B,IMPORT_EXPORT_CHUNK_SIZE);
        end;
        S.Flush;
      finally
        S.Destroy;
      end;
      Log.Send(mtInfo,'Imported file %s',[Filename]);
    finally
      EndOperation;
    end;
end;

// Export Filename to Stream
procedure TFileContainer.ExportFile(Stream: TStream; Filename: String);
var
  S: TStreamFileHandle;
  B: Pointer;
  L: Integer;
  F: TFile;
begin
  if not Assigned(Stream) then
    raise EFileError.Create('Stream not assigned');
  Active := True;
  if BeginOperation(foExport) then
    try
      F := FFiles.Find(Filename);
      if F = nil then
        raise EFileError.CreateFmt('Stream %s not found',[Filename]);
      S := F.CreateHandle(fkStream) as TStreamFileHandle;
      try
        S.Seek(0);
        Stream.Position := 0;
        Stream.Size := S.Size;
        DoProgress(Filename,0,S.Size);
        B := GetMem(IMPORT_EXPORT_CHUNK_SIZE);
        try
          while S.Size - S.Position >= IMPORT_EXPORT_CHUNK_SIZE do
            begin
              S.Read(B^,IMPORT_EXPORT_CHUNK_SIZE);
              S.Flush;
              Stream.Write(B^,IMPORT_EXPORT_CHUNK_SIZE);
              DoProgress(Filename,S.Position,S.Size);
            end;
          L := S.Size - S.Position;
          S.Read(B^,L);
          Stream.Write(B^,L);
          DoProgress(Filename,S.Size,S.Size);
        finally
          FreeMem(B,IMPORT_EXPORT_CHUNK_SIZE);
        end;
        //S.Flush;
      finally
        S.Destroy;
      end;
      Log.Send(mtInfo,'Exported file %s',[Filename]);
    finally
      EndOperation;
    end;
end;

// METADATA

// Loads metadata from disk and stores it in a stream.  Loads file list from the stream then
// calls DoLoadMetadata so custom metadata can be loaded
procedure TFileContainer.LoadMetadata;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    FMetadata.LoadToStream(Stream);
    Stream.Position := 0;
    FFiles.LoadFromStream(Stream);
    DoLoadMetadata(Stream);
  finally
    Stream.Destroy;
  end;
  FMetadata.Changed := False;
end;

// If metadata has changed, saves metadata to a stream and then saves that stream to disk.
// Calls DoSaveMetadata so custom metadata can be saved
procedure TFileContainer.SaveMetadata;
var
  Stream: TMemoryStream;
begin
  if not FMetadata.Changed then Exit;
  Stream := TMemoryStream.Create;
  try
    FFiles.SaveToStream(Stream);
    DoSaveMetadata(Stream);
    FMetadata.SaveFromStream(Stream);
  finally
    Stream.Destroy;
  end;
  FMetadata.Changed := False;
end;

// Clears the file list and notifies the component that metadata has changed.
// Override in descendants to clear any custom metadata
procedure TFileContainer.ClearMetadata;
begin
  FFiles.Clear;
  FMetadata.Changed := True;
end;

// Trigger the OnLoadMetadata event handler.  Override to load custom metadata from the metadata stream
procedure TFileContainer.DoLoadMetadata(Stream: TStream);
begin
  if Assigned(FOnLoadMetadata) then
    FOnLoadMetadata(Self,Stream);
end;

// Trigger the OnSaveMetadata event handler.  Override to save custom metadata to the metadata stream
procedure TFileContainer.DoSaveMetadata(Stream: TStream);
begin
  if Assigned(FOnSaveMetadata) then
    FOnSaveMetadata(Self,Stream);
end;

// PASSWORDS/ENCRYPTION

// Returns true if a password is required to access a file.
// Returns false when the file is not active.
function TFileContainer.PasswordRequired: Boolean;
begin
  if Active then
    Result := FHeader.Hash <> haNone
  else
    Result := False;
end;

// Sets the password property.
// If the file is not active then the password will be tried when a password protected file is opened
// If the file is active then also set the password hash in the header (change access password)
procedure TFileContainer.SetPassword(AValue: String);
var
  Hash     : THash;
  Digest   : Pointer;
  HashSize : Integer;
begin
  FPassword:=AValue;
  if Active then
    begin
      if FPassword = '' then
        if Encrypted then
          raise EFileError.Create('An encrypted file requires a password')
        else
          begin
            FHeader.Hash := haNone;
            FHeader.Digest := ZERO_DIGEST;
            FillChar(FHeader.Key,SizeOf(FHeader.Key),0);
            FillChar(FKey,SizeOf(FKey),0);
          end
      else
        begin
          Hash := CreateHash(PASSWORD_HASH);
          try
            HashSize := Hash.HashSize div 8;
            Digest := GetMem(HashSize);
            try
              Hash.Init;
              Hash.UpdateStr(AValue);
              Hash.Final(Digest^);
              if HashSize > SizeOf(FHeader.Digest) then   // Hash can be larger than 128 bits but only first 128 bits are used
                HashSize := SizeOf(FHeader.Digest);
              Move(Digest^,FHeader.Digest,HashSize);
              Log.Send(mtDebug,'SetPassword Digest='+BufferToHex(@FHeader.Digest,HashSize));
              FHeader.Hash := PASSWORD_HASH;
              SaveCipherKey;
            finally
              FreeMem(Digest);
            end;
          finally
            Hash.Free;
          end;
        end;
      Log.Send(mtInfo,'Access password changed');
    end;
end;

// Returns TRUE if AValue is the correct password for the file.
// Return FALSE if the file is not active.
function TFileContainer.CheckPassword(AValue: String): Boolean;
var
  Hash: THash;
  Digest: Pointer;
  Size: Integer;
begin
  if Active then
    if FHeader.Hash = haNone then
      Result := False
    else
      begin
        Hash := CreateHash(FHeader.Hash);
        try
          Size := Hash.HashSize div 8;
          Digest := GetMem(Size);
          try
            Hash.Init;
            Hash.UpdateStr(AValue);
            Hash.Final(Digest^);
            if Size > SizeOf(FHeader.Digest) then
              Size := SizeOf(FHeader.Digest);
            Result := CompareMem(Digest,@FHeader.Digest,Size) = 0;
          finally
            FreeMem(Digest);
          end;
        finally
          Hash.Free;
        end;
    end
  else
    Result := False;
end;

// Logs into the file and returns true if access is granted.
// If a password is not required then returns TRUE.
// Tries the value of Password first.  If that doesn't work, trigger the OnGetPassword event handler to
// retrieve a password.
// Try to retrieve the password up to 3 times.  To exit the loop return a Password of '' in the OnGetPassword
// event handler.
function TFileContainer.Login: Boolean;
var
  Count: Integer;
begin
  // If the Digest field in the header is all zeros then authentication passed
  if PasswordRequired then
    begin
      // Try the password stored in FPassword first
      Result := CheckPassword(FPassword);
      // If that failed then use the event handler to get the password
      if (not Result) and Assigned(FOnGetPassword) then
        begin
          Count := 0;
          repeat
            FOnGetPassword(Self,FPassword);
            if FPassword > '' then
              Result := CheckPassword(FPassword)
            else
              Break;
            inc(Count)
          until Result or (Count = 3);
        end;
    end
  else
    Result := True;
end;

// Returns true if the file is open and encrypted
function TFileContainer.GetEncrypted: Boolean;
begin
  Result := Active and (FHeader.Cipher <> caNone);
end;

// Loads the key from the header and decrypts it.  Requires a Password and Password hash to be set.
procedure TFileContainer.LoadCipherKey;
var
  Hash   : THash;
  Digest : Pointer;
  LCipher : TCipher;
begin
  Hash := CreateHash(KEY_ENCRYPTION_HASH);
  try
    Digest := GetMem(Hash.HashSize div 8);
    try
      Hash.Init;
      Hash.UpdateStr(FPassword);
      Hash.Final(Digest^);
      LCipher := CreateCipher(KEY_ENCRYPTION_CIPHER,Digest,Hash.HashSize);
      try
        LCipher.InitMode(KEY_ENCRYPTION_MODE,@FHeader.Digest);
        FKey := FHeader.Key;
        //Log.Send(mtDebug,'LoadCipherKeyEnc '+BufferToHex(@FKey,SizeOf(FKey)));
        LCipher.Decrypt(@FKey,SizeOf(FKey));
        //Log.Send(mtDebug,'LoadCipherKeyDec '+BufferToHex(@FKey,SizeOf(FKey)));
      finally
        LCipher.Free;
      end;
    finally
      FreeMem(Digest);
    end;
  finally
    Hash.Free;
  end;
end;

// Encrypts the key and saves it to the header.  Requires a password and password hash to be set.
procedure TFileContainer.SaveCipherKey;
var
  Hash   : THash;
  Digest : Pointer;
  LCipher : TCipher;
begin
  Hash := CreateHash(KEY_ENCRYPTION_HASH);
  try
    Digest := GetMem(Hash.HashSize div 8);
    try
      Hash.Init;
      Hash.UpdateStr(FPassword);
      Hash.Final(Digest^);
      LCipher := CreateCipher(KEY_ENCRYPTION_CIPHER,Digest,Hash.HashSize);
      try
        LCipher.InitMode(KEY_ENCRYPTION_MODE,@FHeader.Digest);
        FHeader.Key := FKey;
        Log.Send(mtDebug,'SaveCipherKeyDec '+BufferToHex(@FKey,SizeOf(FKey)));
        LCipher.Encrypt(@FHeader.Key,SizeOf(FKey));
        Log.Send(mtDebug,'SaveCipherKeyEnc '+BufferToHex(@FHeader.Key,SizeOf(FKey)));
      finally
        LCipher.Free;
      end;
    finally
      FreeMem(Digest);
    end;
  finally
    Hash.Free;
  end;
end;

// Loads the key from the file, decrypts it, and initializes the cipher.
procedure TFileContainer.InitCipherKey;
begin
  LoadCipherKey;
  if Assigned(FCipher) then
    FreeAndNil(FCipher);
  //Log.Send(mtDebug,'Creating %s cipher with key=%s',[CIPHER_ALGORITHM_STR[FHeader.Cipher],BufferToHex(@FKey,SizeOf(FKey))]);
  FCipher := CreateCipher(FHeader.Cipher,@FKey,SizeOf(FKey) * 8);
end;

// Generates a random key and initializes the cipher.  Encrypts the key and saves it to the header.  Also generates a random IV.
procedure TFileContainer.GenerateCipherKey;
begin
  FillRandom(@FKey,SizeOf(FKey));
  //Log.Send(mtDebug,'GenerateCipherKey = '+BufferToHex(@FKey,SizeOf(FKey)));
  SaveCipherKey;
  InitCipherKey;
end;

function TFileContainer.GetCipherAlgorithm: TCipherAlgorithm;
begin
  if Assigned(FCipher) then
    Result := FCipher.Algorithm
  else
    Result := caNone;
end;

// Sets the cipher algorithm for the file to the one identified by AValue.
// Throws an exception if the file is already encrypted.
{procedure TFileContainer.SetCipherAlgorithm(AValue: TCipherAlgorithm);
begin
  if AValue = GetCipherAlgorithm then
    Exit;
  if Encrypted then
    raise EFileError.Create('To change the cipher algorithm, first decrypt the file.');
  if Assigned(FCipher) then
    FreeAndNil(FCipher);
  if AValue <> caNone then
    begin
      FCipher := CreateCipher(AValue);
      InitCipherKey;
    end;
  FHeader.Algorithm := AValue;
end; }

procedure TFileContainer.EncryptPage(PageNum: Cardinal; Data: Pointer);
var
  BlockSize: Integer;
  HashSize: Integer;
  Digest,IV: Pointer;
  Hash: THash;
begin
  // The page IV is the MD4 hash of the PageNum
  BlockSize := FCipher.BlockSize div 8;
  IV := GetMem(BlockSize);
  try
    FillChar(IV^,BlockSize,0);
    Hash := CreateHash(haMD4);
    try
      HashSize := Hash.HashSize div 8;
      Digest := GetMem(HashSize);
      try
        Hash.Init;
        Hash.Update(PageNum,SizeOf(PageNum));
        Hash.Final(Digest^);
        if HashSize > BlockSize then
          Move(Digest^,IV^,BlockSize)
        else
          Move(Digest^,IV^,HashSize);
        FCipher.InitMode(cmCFB,IV);
        FCipher.Encrypt(Data,PAGE_SIZE);
      finally
        FreeMem(Digest);
      end;
    finally
      Hash.Free;
    end;
  finally
    FreeMem(IV);
  end;
end;

procedure TFileContainer.DecryptPage(PageNum: Cardinal; Data: Pointer);
var
  BlockSize: Integer;
  HashSize: Integer;
  Digest, IV: Pointer;
  Hash: THash;
begin
  // The page IV is the MD4 hash of the PageNum
  BlockSize := FCipher.BlockSize div 8;
  IV := GetMem(BlockSize);
  try
    FillChar(IV^,BlockSize,0);
    Hash := CreateHash(haMD4);
    try
      HashSize := Hash.HashSize div 8;
      Digest := GetMem(HashSize);
      try
        Hash.Init;
        Hash.Update(PageNum,SizeOf(PageNum));
        Hash.Final(Digest^);
        if HashSize > BlockSize then
          Move(Digest^,IV^,BlockSize)
        else
          Move(Digest^,IV^,HashSize);
        FCipher.InitMode(cmCFB,IV);
        FCipher.Decrypt(Data,PAGE_SIZE);
      finally
        FreeMem(Digest);
      end;
    finally
      Hash.Free;
    end;
  finally
    FreeMem(IV);
  end;
end;

// Encrypts the file
procedure TFileContainer.Encrypt(Password: String; Algorithm: TCipherAlgorithm = caTwoFish);
var
  X: Integer;
  D: Pointer;
begin
  if Encrypted then
    begin
      Log.Send(mtDebug,'Already encrypted');
      Exit;
    end;
  if Password = '' then
    raise EFileError.Create('Encryption requires a password');
  if BeginOperation(foEncrypt) then
    try
      if Assigned(FCipher) then
        FreeAndNil(FCipher);
      FHeader.Cipher := Algorithm;
      FEncrypting := True;
      SetPassword(Password);
      GenerateCipherKey;
      DoProgress(Filename,0,FPageCount-1);
      Flush;
      GetMem(D,PAGE_SIZE);
      try
        for X := 1 to FPageCount - 1 do
          begin
            ReadData(X,D);
            EncryptPage(X,D);
            WriteData(X,D);
            DoProgress(Filename,X,FPageCount - 1);
          end;
        FEncrypting := False;
      finally
        FreeMem(D,PAGE_SIZE);
      end;
    finally
      EndOperation;
    end;
end;

// Decrypts the file using the currently selected encryptor
procedure TFileContainer.Decrypt;
var
  X: Integer;
  D: Pointer;
begin
  if not Encrypted then Exit;
  if BeginOperation(foDecrypt) then
    try
      DoProgress(Filename,0,FPageCount - 1);
      Flush;
      FHeader.Hash := haNone;
      FHeader.Cipher := caNone;
      GetMem(D,PAGE_SIZE);
      try
        for X := 1 to FPageCount - 1 do
          begin
            ReadData(X,D);
            DecryptPage(X,D);
            WriteData(X,D);
            DoProgress(Filename,X,FPageCount - 1);
          end;
      finally
        FreeMem(D,PAGE_SIZE);
      end;
      FillChar(FHeader.Digest,SizeOf(TFileDigest),0);
      FillChar(FHeader.Key,SizeOf(TFileKey),0);
      FPassword := '';
      Flush;
    finally
      EndOperation;
    end;
end;

// PACK Operation

// Sends a PageNumChanged message to objects in the component.  Used by Pack()
procedure TFileContainer.NotifyPageNumChanged(OldPageNum, NewPageNum: TPageNum);
begin
  if FFreePages.PageNumChanged(OldPageNum,NewPageNum) then Exit;
  if FMetadata.PageNumChanged(OldPageNum,NewPageNum) then Exit;
  if FFiles.PageNumChanged(OldPageNum,NewPageNum) then Exit;
end;

// Deletes any pages at the end of the container that are in the free page list.  Used by Pack()
procedure TFileContainer.DeleteTrailingFreePages;
begin
  Log.Send(mtDebug,'DeleteTrailingFreePages');
  FreePages.Truncate(FPageCount);
  while (FreePages.Count > 0) and (FHeader.FreePages[1] = FPageCount - 1) do
    Truncate(FPageCount - 1);
end;

// Replace the target page with the source page.  Used by Pack.
procedure TFileContainer.Replace(Source, Target: TPageNum);
var
  Page: PBufferedPage;
begin
  Page := Fetch(Source);
  Page^.PageNum := Target;
  Page^.Modified := True;
  Release(Page);
  NotifyPageNumChanged(Source,Target);
  if Source = PageCount - 1 then
    Truncate(Source)
  else
    Delete(Source);
end;

function TFileContainer.BeginOperation(Operation: TFileOperation): Boolean;
begin
  if Operation = foNone then Exit;
  if (FOperation <> foNone)  then
    raise Exception.Create('A file operation is already in progress');
  Result := FOperation = foNone;
  FOperation := Operation;
end;

procedure TFileContainer.EndOperation;
begin
  if FOperation <> foNone then
    Flush;
  FOperation := foNone;
end;

// Remove any free pages at the end of the file and then swap the last non-free
// page into a free page closer to the beginning of the file.  Repeat up to MaxIterations times.
procedure TFileContainer.Pack(MaxIterations: Byte = DEFAULT_PACK_ITERATIONS);
var
  I: Integer;
  Source, Target: TPageNum;
begin
  Log.Send(mtDebug,'Pack');
  Flush;
  for I := 1 to MaxIterations do
    begin
      DeleteTrailingFreePages;
      if FreePages.Count > 0 then
        begin
          Source := PageCount - 1;
          if Source = FDeletingPage then Exit;  { Abort the pack routine if the last page of the file is being deleted }
          Target := FreePages.GetLowest;
          Replace(Source,Target);
        end;
      if FreePages.Count = 0 then
        Break;
    end;
  DeleteTrailingFreePages;
end;

// DEFRAG Operation

// Main routine for sending a PageSwap notification to objects in the component
procedure TFileContainer.NotifyPageSwap(PageNum1, PageNum2: TPageNum);
begin
  FFreePages.PageNumSwapped(PageNum1,PageNum2);
  FMetadata.PageNumSwapped(PageNum1,PageNum2);
  FFiles.PageNumSwapped(PageNum1,PageNum2);
end;

// Performs a page swap and sends a PageSwap notification to objects in the component
procedure TFileContainer.Swap(Source,Target: PBufferedPage);
var
  S,T: Cardinal;
begin
  S := Source^.PageNum;
  T := Target^.PageNum;
  NotifyPageSwap(S,T);
  if Source^.PageNum = S then
    Source^.PageNum := T;
  if Target^.PageNum = T then
    Target^.PageNum := S;
  Target^.Modified := True;
  Source^.Modified := True;
end;

// Performs a defrag operation, rewriting the entire file from beginning to end such that all
// files are stored sequentially by swapping physical pages on disk.
procedure TFileContainer.Defrag;
var
  X,Y: Integer;
  N: Cardinal;
  FI: TFile;
  S,T: PBufferedPage;
begin
  Log.Send(mtInfo,'Defragmenting file container %s',[FFilename]);
  if BeginOperation(foDefrag) then
    try
      DoProgress(Filename,0,PageCount);
      N := 0;
      for X := 0 to FFiles.Count - 1 do
        begin
          FI := FFiles[X];
          for Y := 0 to FI.PageCount - 1 do
            begin
              S := FI.Fetch(Y);
              inc(N);
              T := Fetch(N);
              if S^.PageNum <> T^.PageNum then
                Swap(S,T);
              FI.Release(S);
              Release(T);
              FI.Flush;
              DoProgress(Filename,N,PageCount);
            end;
        end;
      DoProgress(Filename,PageCount,PageCount);
      Log.Send(mtInfo,'File container %s has been defragmented',[FFilename]);
    finally
      EndOperation;
    end;
end;

// MISCELANEOUS PROPERTIES

// Return the file version from the header
function TFileContainer.GetVersion: Byte;
begin
  Result := FHeader.Version;
end;

// Calculate the container file size
function TFileContainer.GetFileSize: Cardinal;
begin
  Result := FPageCount * PAGE_SIZE;
end;

// Triggers the OnProgress event handler
procedure TFileContainer.DoProgress(AName: String; APosition,ASize: Cardinal);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self,Operation,AName,APosition,ASize);
end;

{$IFDEF DEBUG}

// Validates buffers, free pages and page map.  Raises an exception if an error is found.
procedure TFileContainer.Validate;
begin
  ValidateBuffers;
  ValidateFreePages;
  ValidatePageMap;
end;

// Check the integrity of the file buffers, raising an exception if an error is found.
procedure TFileContainer.ValidateBuffers;
var
  I: Integer;
begin
  for I := 0 to FFiles.Count - 1 do
    with FFiles[I] do
      begin
        FBuffer.CheckOrder;
        FBuffer.CheckRange;
      end;
end;

// Validates the page map and raises an exception if an error is found
procedure TFileContainer.ValidatePageMap;
begin
  PageMap.Build;
  PageMap.Validate;
  Flush;
end;

// Validates the free page list and raises an exception if an error is found
procedure TFileContainer.ValidateFreePages;
begin
  FreePages.CheckOrder;
end;
{$ENDIF}

end.

