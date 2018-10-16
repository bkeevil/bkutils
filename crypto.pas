unit Crypto;

{$MODE Delphi}

interface

uses
  Classes, Sysutils, DCPcrypt2, DCPBase64;

type
  {$IFNDEF FPC}
  Pbyte= ^byte;
  Pword= ^word;
  Pdword= ^dword;
  Pint64= ^int64;
  dword= LongWord;
  Pwordarray= ^Twordarray;
  Twordarray= array[0..19383] of word;
  {$ENDIF}
  Pdwordarray= ^Tdwordarray;
  Tdwordarray= array[0..8191] of dword;


  TBlock64 = array[0..1] of DWORD;
  TBlock128 = array[0..3] of DWORD;
  TBlock256 = array[0..7] of DWORD;
  PBlock64 = ^TBlock64;
  PBlock128 = ^TBlock128;
  PBlock256 = ^TBlock256;

  TKey128 = TBlock128;
  TKey256 = TBlock256;
  PKey128 = ^TKey128;
  PKey256 = ^TKey256;

  TCipherAlgorithm  = (caNone, caBlowfish, caTwofish, caRC2, {caRC4, }caRC5, caRC6, caDES, ca3DES, caCast128, caCast256, caGOST, caMars, caRijndael, caIDEA, caICE, caThinICE, caICE2, caSerpent, caTEA, caMisty1);
  TCipherMode       = (cmCBC, cmCFB8Bit, CFBBlock, cmOFB, cmCTR);
  THashAlgorithm    = (haNone, haMD4, haMD5, haSHA1, haSHA256, haSHA384, haSHA512, haRipeMD128, haRipeMD160, haHaval256, haTiger);

const
  CIPHER_ALGORITHM_STR: array[TCipherAlgorithm] of String = ('None','Blowfish','Twofish','RC2',{'RC4',}'RC5','RC6','DES','3DES','CAST-128','CAST-256','GOST','Mars','Rijndael','IDEA','ICE','ThinIce','ICE2','Serpent','TEA','Misty1');
  CIPHER_MODE_STR: array[TCipherMode] of String = ('CBC','CFB-8Bit','CFB-Block','OFB','CTR');
  HASH_ALGORITHM_STR: array[THashAlgorithm] of String = ('None','MD4','MD5','SHA1','SHA-256','SHA-384','SHA-512','RipeMD-128','RipeMD-160','Haval-256','Tiger');

type
  TMAC128 = TBlock128;
  TMAC64  = TBlock64;

function GetCipherBlockSize(Kind: TCipherAlgorithm): Integer;
function GetCipherClass(Kind: TCipherAlgorithm): TDCP_BlockCipherClass;
function CreateCipher(Kind: TCipherAlgorithm; AOwner: TComponent = nil): TDCP_BlockCipher;
function CreateHash(Kind: THashAlgorithm; AOwner: TComponent): TDCP_Hash;
procedure EncryptStream(InStream, OutStream: TStream; Password: String); overload;
procedure EncryptStream(InStream, OutStream: TStream; Key: TBlock128); overload;
procedure DecryptStream(InStream, OutStream: TStream; Password: String); overload;
procedure DecryptStream(InStream, OutStream: TStream; Key: TBlock128); overload;
function EncryptString(const InString: String; Password: String): String;
function DecryptString(const InString: String; Password: String): String;
function MD5String(Str: String): TBlock128;
function SHA256String(Str: String): TBlock256;
function Block256ToString(Digest: TBlock256): String;
function Block128ToString(Digest: TBlock128): String;
function Block256ToBase64(Digest: TBlock256): String;
function Block128ToBase64(Digest: TBlock128): String;
function HMAC128(Algorithm: THashAlgorithm; Data: Pointer; DataSize: Integer; Key: Pointer; KeySize: Integer): TMAC128;
function HMAC64(Algorithm: THashAlgorithm; Data: Pointer; DataSize: Integer; Key: Pointer; KeySize: Integer): TMAC64;
procedure XORMem(A,B,R: Pointer; Size: Integer);
procedure XOR64(A,B,R: Pointer);
procedure XOR128(A,B,R: Pointer);
procedure Inc64(V: Pointer);
procedure Inc128(V: Pointer);
function CompareMem(A,B: Pointer; Size: Integer): Integer;
function BufferToHex(Buffer: Pointer; Size: Integer): String;
function LFSR(var N: Cardinal): Cardinal;
function ROTL32(var X: Cardinal; Y: Byte = 1): Cardinal;
function ROTR32(var X: Cardinal; Y: Byte = 1): Cardinal;
function XpowYmodN(X, Y, N: Cardinal): Cardinal;
function GeneratePrime: Cardinal;
function MillerRabin(N: Cardinal; Trials: Byte): Boolean;

implementation

uses
  Base32, Base64, Rand,
  DCPBlowfish, DCPTwoFish, DCPCast128, DCPCast256, DCPDES, DCPICE, DCPIdea, DCPRC2, DCPRC5, DCPRC6,
  DCPRijndael, DCPSerpent, DCPTEA, DCPGOST, DCPMARS, DCPMisty1, DCPRC4,

  DCPHaval, DCPMD4, DCPMD5, DCPRipeMD128, DCPRipeMD160,
  DCPSHA1, DCPSHA256, DCPSHA512, DCPTiger;

function GetCipherBlockSize(Kind: TCipherAlgorithm): Integer;
begin
  Result := GetCipherClass(Kind).GetBlockSize;
end;

function GetCipherClass(Kind: TCipherAlgorithm): TDCP_BlockCipherClass;
begin
  case Kind of
    caBlowfish : Result := TDCP_Blowfish;
    caTwofish  : Result := TDCP_Twofish;
    caRC2      : Result := TDCP_RC2;
    caRC5      : Result := TDCP_RC5;
    //caRC4      : Result := TDCP_RC4;
    caRC6      : Result := TDCP_RC6;
    caDES      : Result := TDCP_DES;
    ca3DES     : Result := TDCP_3DES;
    caCast128  : Result := TDCP_Cast128;
    caCast256  : Result := TDCP_Cast256;
    caRijndael : Result := TDCP_Rijndael;
    caIDEA     : Result := TDCP_IDEA;
    caICE      : Result := TDCP_ICE;
    caThinICE  : Result := TDCP_ThinICE;
    caICE2     : Result := TDCP_ICE2;
    caSerpent  : Result := TDCP_Serpent;
    caTEA      : Result := TDCP_TEA;
    caMisty1   : Result := TDCP_Misty1;
    caMars     : Result := TDCP_Mars;
    caGOST     : Result := TDCP_GOST;
  else
    raise EDCP_BlockCipher.Create('Cipher not found');
  end;
end;

function CreateCipher(Kind: TCipherAlgorithm; AOwner: TComponent): TDCP_BlockCipher;
begin
  GetCipherClass(Kind).Create(AOwner);
end;

function CreateHash(Kind: THashAlgorithm; AOwner: TComponent): TDCP_Hash;
begin
  case Kind of
    haNone      : Result := nil;
    haMD4       : Result := TDCP_MD4.Create(AOwner);
    haMD5       : Result := TDCP_MD5.Create(AOwner);
    haSHA1      : Result := TDCP_SHA1.Create(AOwner);
    haSHA256    : Result := TDCP_SHA256.Create(AOwner);
    haSHA384    : Result := TDCP_SHA384.Create(AOwner);
    haSHA512    : Result := TDCP_SHA512.Create(AOwner);
    haRipeMD128 : Result := TDCP_RipeMD128.Create(AOwner);
    haRipeMD160 : Result := TDCP_RipeMD160.Create(AOwner);
    haHaval256  : Result := TDCP_Haval.Create(AOwner);
    haTiger     : Result := TDCP_Tiger.Create(AOwner);
  else
    raise EDCP_Hash.Create('Hash algorithm not found');
  end;
end;

procedure EncryptStream(InStream, OutStream: TStream; Password: String);
var
  Cipher: TDCP_RC4;
begin
  InStream.Position := 0;
  OutStream.Size := 0;
  Cipher := TDCP_RC4.Create(nil);
  try
    Cipher.InitStr(Password,TDCP_SHA512);
    Cipher.EncryptStream(InStream,OutStream,InStream.Size);
  finally
    Cipher.Free;
  end;
end;

procedure EncryptStream(InStream, OutStream: TStream; Key: TBlock128);
var
  Cipher: TDCP_RC4;
begin
  InStream.Position := 0;
  OutStream.Size := 0;
  Cipher := TDCP_RC4.Create(nil);
  try
    Cipher.Init(Key,SizeOf(Key),nil);
    Cipher.EncryptStream(InStream,OutStream,InStream.Size);
  finally
    Cipher.Free;
  end;
end;

procedure DecryptStream(InStream, OutStream: TStream; Password: String);
var
  Cipher: TDCP_RC4;
begin
  InStream.Position := 0;
  OutStream.Size := 0;
  Cipher := TDCP_RC4.Create(nil);
  try
    Cipher.InitStr(Password,TDCP_SHA512);
    Cipher.DecryptStream(InStream,OutStream,InStream.Size);
  finally
    Cipher.Free;
  end;
end;

procedure DecryptStream(InStream, OutStream: TStream; Key: TBlock128);
var
  Cipher: TDCP_RC4;
begin
  InStream.Position := 0;
  OutStream.Size := 0;
  Cipher := TDCP_RC4.Create(nil);
  try
    Cipher.Init(Key,SizeOf(Key),nil);
    Cipher.DecryptStream(InStream,OutStream,InStream.Size);
  finally
    Cipher.Free;
  end;
end;

function EncryptString(const InString: String; Password: String): String;
var
  Cipher: TDCP_RC4;
begin
  Cipher := TDCP_RC4.Create(nil);
  try
    Cipher.InitStr(Password,TDCP_SHA512);
    Result := Cipher.EncryptString(InString);
  finally
    Cipher.Free;
  end;
end;

function DecryptString(const InString: String; Password: String): String;
var
  Cipher: TDCP_RC4;
begin
  Cipher := TDCP_RC4.Create(nil);
  try
    Cipher.InitStr(Password,TDCP_SHA512);
    Result := Cipher.DecryptString(InString);
  finally
    Cipher.Free;
  end;
end;

procedure Hash(Algorithm: THashAlgorithm; Data: Pointer; DataSize: Integer; out Hash: Pointer; out HashSize: Integer);
var
  H: TDCP_Hash;
begin
  H := CreateHash(Algorithm,nil);
  try
    HashSize := H.HashSize div 8;
    Hash := GetMem(HashSize);
    H.Init;
    H.Update(Data^,DataSize);
    H.Final(Hash^);
    H.Burn;
  finally
    H.Free;
  end;
end;

function MD5String(Str: String): TBlock128;
var
  Hash: TDCP_Hash;
  Size: Integer;
begin
  Hash := CreateHash(haMD5,nil);
  try
    Size := Hash.HashSize div 8;
    Assert(Size = SizeOf(Result));
    FillChar(Result,SizeOf(Result),0);
    Hash.Init;
    Hash.UpdateStr(Str);
    Hash.Final(Result);
    Hash.Burn;
  finally
    Hash.Free;
  end;
end;

function SHA256String(Str: String): TBlock256;
var
  Hash: TDCP_Hash;
  Size: Integer;
begin
  Hash := CreateHash(haSHA256,nil);
  try
    Size := Hash.HashSize div 8;
    Assert(Size = SizeOf(Result));
    FillChar(Result,SizeOf(Result),0);
    Hash.Init;
    Hash.UpdateStr(Str);
    Hash.Final(Result);
    Hash.Burn;
  finally
    Hash.Free;
  end;
end;

function Block128ToString(Digest: TBlock128): String;
begin
  SetLength(Result,SizeOf(Digest));
  Move(Digest,Result,SizeOf(Digest));
end;

function Block256ToString(Digest: TBlock256): String;
begin
  SetLength(Result,SizeOf(Digest));
  Move(Digest,Result,SizeOf(Digest));
end;

function Block256ToBase64(Digest: TBlock256): String;
begin
  SetLength(Result,((SizeOf(Digest)+2) div 3) * 4);
  Base64Encode(@Digest[0],@Result[1],SizeOf(Digest));
end;

function Block128ToBase64(Digest: TBlock128): String;
begin
  SetLength(Result,((SizeOf(Digest)+2) div 3) * 4);
  Base64Encode(@Digest[0],@Result[1],SizeOf(Digest));
end;

function HMAC128(Algorithm: THashAlgorithm; Data: Pointer; DataSize: Integer; Key: Pointer; KeySize: Integer): TMAC128;
var
  H: TDCP_Hash;
  IKey: Pointer;
  IKeySize: Integer;
  FreeIKey: Boolean;
  IPad: Pointer;
  OPad: Pointer;
  Digest: Pointer;
begin
  // If the key size is over 64 bytes long then use a hash of it instead
  if KeySize > 64 then
    begin
      Hash(Algorithm,Key,KeySize,IKey,IKeySize);
      FreeIKey := True;
    end
  else
    begin
      IKey := Key;
      IKeySize := KeySize;
      FreeIKey := False;
    end;
  try
    IPad := GetMem(64);
    OPad := GetMem(64);
    try
      Assert(KeySize <= 64);
      FillChar(IPad^,64,$36);
      XORMem(IPad,Key,IPad,KeySize);
      FillChar(OPad^,64,$5C);
      XORMem(OPad,Key,OPad,KeySize);
      H := CreateHash(Algorithm,nil);
      try
        Assert(H.HashSize mod 8 = 0);
        Digest := GetMem(H.HashSize div 8);
        try
          H.Init;
          H.Update(IPad^,64);
          H.Update(Data^,DataSize);
          H.Final(Digest^);
          H.Burn;
          H.Init;
          H.Update(OPad^,64);
          H.Update(Digest^,H.HashSize div 8);
          H.Final(Digest^);
          Move(Digest^,Result,SizeOf(Result));
          H.Burn;
        finally
          FreeMem(Digest);
        end;
      finally
        FreeAndNil(H);
      end;
    finally
      FreeMem(IPad);
      FreeMem(OPad);
    end;
  finally
    if FreeIKey then
      FreeMem(IKey);
  end;
end;

function HMAC64(Algorithm: THashAlgorithm; Data: Pointer; DataSize: Integer; Key: Pointer; KeySize: Integer): TMAC64;
var
  MAC128: TMAC128;
begin
  MAC128 := HMAC128(Algorithm,Data,DataSize,Key,KeySize);
  Result[0] := MAC128[0];
  Result[1] := MAC128[1];
end;

procedure XORMem(A,B,R: Pointer; Size: Integer);
var
  PA,PB,PR: PByte;
  X: Word;
begin
  for X := 0 to Size - 1 do
    begin
      PA := PByte(PtrInt(A)+X);
      PB := PByte(PtrInt(B)+X);
      PR := PByte(PtrInt(R)+X);
      PR^ := PA^ xor PB^;
    end;
end;

procedure XOR64(A,B,R: Pointer);
begin
  PBlock64(R)[0] := PBlock64(A)[0] xor PBlock64(B)[0];
  PBlock64(R)[1] := PBlock64(A)[1] xor PBlock64(B)[1];
end;

procedure XOR128(A,B,R: Pointer);
begin
  PBlock128(R)[0] := PBlock128(A)[0] xor PBlock128(B)[0];
  PBlock128(R)[1] := PBlock128(A)[1] xor PBlock128(B)[1];
  PBlock128(R)[2] := PBlock128(A)[2] xor PBlock128(B)[2];
  PBlock128(R)[3] := PBlock128(A)[3] xor PBlock128(B)[3];
end;

procedure Inc64(V: Pointer);
begin
  inc(PBlock64(V)[1]);
  if PBlock64(V)[1] = 0 then
    inc(PBlock64(V)[0]);
end;

procedure Inc128(V: Pointer);
var
  i: integer;
begin
  Inc(PBlock128(V)[3]);
  i:= 3;
  while (i> 0) and (PBlock128(V)[i] = 0) do
    begin
      Inc(PBlock128(V)[i-1]);
      Dec(i);
    end;
end;

function CompareMem(A, B: Pointer; Size: Integer): Integer;
var
  X: Integer;
  PA, PB: PByte;
begin
  Result := 0;
  for X := 0 to Size - 1 do
    begin
      PA := PByte(PtrUInt(A) + X);
      PB := PByte(PtrUInt(B) + X);
      if PA^ > PB^ then
        begin
          Result := 1;
          Exit;
        end
      else
        if PB^ > PA^ then
          begin
            Result := -1;
            Exit;
          end;
    end;
end;

function BufferToHex(Buffer: Pointer; Size: Integer): String;
var               { Fails on large strings }
  X: Integer;
  B: Byte;
  S: String;
begin
  Result := '';
  for X := 0 to Size - 1 do
    begin
      B := PByte(PtrUInt(Buffer) + X)^;
      S := IntToHex(B,2)+' ';
      Result := Result + S;
    end;
end;

// Linear Feedback Shift Register
function LFSR(var N: Cardinal): Cardinal;
begin
  if (N and 1) > 0 then
    N := ((N xor $80000055) shr 1) or $80000000
  else
    N := N shr 1;
  Result := N;
end;

function ROTL32(var X: Cardinal; Y: Byte = 1): Cardinal;
begin
  X := (X shl Y) or (X shr (32-Y));
end;

function ROTR32(var X: Cardinal; Y: Byte = 1): Cardinal;
begin
  X := (X shr Y) or (x shl (32 -Y));
end;

// Raises X to the power Y in modulus N
function XpowYmodN(X, Y, N: Cardinal): Cardinal;
var
  A,B: QWORD;
begin
  A := 1; B := X;
  while Y > 0 do
    begin
      if Y mod 2 = 1 then
        A := (A * B) mod N;
      B := (B * B) mod N;
      Y := Y div 2;
    end;
  Result := A mod N;
end;

// Performs the Miller-Rabin primality test on N
function MillerRabin(N: Cardinal; Trials: Byte): Boolean;
var
  I: Byte;
  A: Cardinal;
begin
  Result := False;
  for I := 1 to Trials do
    begin
      A := (RNG.Generate mod (N-3)) + 2;  // Get random value between 2 and N-1
      if XpowYmodN(A,N-1,N) <> 1 then Exit;    // Is it prime?
    end;
  Result := True;
end;

// Generate a random 32 bit prime number
function GeneratePrime: Cardinal;
begin
  Result := RNG.Generate;
  if (Result and 1) = 0 then inc(Result);
  while not MillerRabin(Result,5) do inc(Result,2);
end;

end.

