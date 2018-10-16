unit Logging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogDispatcher   = class;
  TLogListener     = class;
  TLogMessageType  = (mtInfo, mtWarning, mtDebug, mtError);
  TLogMessageTypes = set of TLogMessageType;
  TLogMessageEvent = procedure (Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String) of object;

const
  ALL_LOG_MESSAGE_TYPES = [mtInfo, mtWarning, mtDebug, mtError];
  DEFAULT_LOG_MESSAGE_TYPES = [mtInfo, mtWarning, mtError];

  MESSAGE_TYPE_STRINGS: array[TLogMessageType] of String = ('INFO','WARNING','DEBUG','ERROR');

type

  { TLogListener }

  TLogListener = class(TObject)
    private
      FEnabled: Boolean;
      FOnMessage: TLogMessageEvent;
      FTypeFilter: TLogMessageTypes;
      FNameFilter: String;
      function FilterMessage(Dispatcher: TLogDispatcher; MessageType: TLogMessageType): Boolean;
      procedure Receive(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
    public
      constructor Create;
      destructor Destroy; override;
      //
      procedure Message(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String); virtual;
      //
      property Enabled: Boolean read FEnabled write FEnabled;
      property TypeFilter: TLogMessageTypes read FTypeFilter write FTypeFilter;
      property NameFilter: String read FNameFilter write FNameFilter;
      property OnMessage: TLogMessageEvent read FOnMessage write FOnMessage;
  end;

  { TLogFileListener }

  TLogFileListener = class(TLogListener)
    private
      FFile: Text;
      FFilename: String;
    public
      procedure Message(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String); override;
    public
      constructor Create(AFilename: String; Append: Boolean = True);
      destructor Destroy; override;
      property Filename: String read FFilename;
  end;

  { TLogCRTListener }

  TLogCRTListener = class(TLogListener)
    public
      procedure Message(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String); override;
  end;

  { TLogStringsListener }

  TLogStringsListener = class(TLogListener)
    private
      FStrings: TStrings;
    public
      procedure Message(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String); override;
    public
      constructor Create(Strings: TStrings);
      property Strings: TStrings read FStrings;
  end;

  { TLogListeners }

  TLogListeners = class(TObject)
    private
      FList: TList;
      function GetCount: Integer;
      function GetListener(Index: Integer): TLogListener;
    public
      constructor Create;
      destructor Destroy; override;
      //
      procedure Add(Listener: TLogListener);
      procedure Remove(Listener: TLogListener);
      //
      property Count: Integer read GetCount;
      property Items[Index: Integer]: TLogListener read GetListener; default;
  end;

  { TLogDispatcher }

  TLogDispatcher = class(TObject)
    private
      FEnabled: Boolean;
      FName: String;
      FFilter: TLogMessageTypes;
    public
      constructor Create(AName: String);
      //
      procedure Send(MessageType: TLogMessageType; Message: String); overload;
      procedure Send(MessageType: TLogMessageType; Message: String; Args: array of const); overload;
      //
      property Enabled: Boolean read FEnabled write FEnabled;
      property Name: String read FName write FName;
      property Filter: TLogMessageTypes read FFilter write FFilter;
   end;

  { TLogObject }

  TLogObject = class(TObject)
    private
      FLog: TLogDispatcher;
    public
      constructor Create;
      destructor Destroy; override;
      //
      property Log: TLogDispatcher read FLog;
  end;

var
  LogListeners: TLogListeners;

function LogMessageString(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String): String;

implementation

uses
  Crt;

function LogMessageString(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String): String;
var
  T,D,M,S: String;
begin
  T := MESSAGE_TYPE_STRINGS[MessageType];
  if Assigned(Dispatcher) then
    D := Dispatcher.Name
  else
    D := '';
  M := Message;
  S := Format('%-18s [%-8s] (%-12s) %s',[DateTimeToStr(Now),T,D,M]);
  Result := S;
end;

{ TLogObject }

constructor TLogObject.Create;
begin
  FLog := TLogDispatcher.Create(Self.ClassName);
end;

destructor TLogObject.Destroy;
begin
  if Assigned(FLog) then
    FLog.Destroy;
  inherited Destroy;
end;

{ TLogDispatcher }

constructor TLogDispatcher.Create(AName: String);
begin
  FEnabled := True;
  FFilter := ALL_LOG_MESSAGE_TYPES;
  FName := AName;
end;

procedure TLogDispatcher.Send(MessageType: TLogMessageType; Message: String);
var
  X: Integer;
  L: TLogListener;
begin
  if FEnabled and (MessageType in FFilter) and Assigned(LogListeners) then
    for X := 0 to LogListeners.Count - 1 do
      begin
        L := LogListeners[X];
        if Assigned(L) then
          L.Receive(Self,MessageType,Message);
      end;
end;

procedure TLogDispatcher.Send(MessageType: TLogMessageType; Message: String; Args: array of const);
begin
  Send(MessageType,Format(Message,Args));
end;

{ TLogListeners }

constructor TLogListeners.Create;
begin
  FList := TList.Create;
  FList.Capacity := 8;
end;

destructor TLogListeners.Destroy;
var
  X: Integer;
  L: TLogListener;
begin
  for X := FList.Count - 1 downto 0 do
    begin
      L := Items[X];
      if Assigned(L) then
        L.Destroy;
    end;
  FList.Clear;
  FList.Destroy;
  inherited Destroy;
end;

function TLogListeners.GetListener(Index: Integer): TLogListener;
begin
  Result := TLogListener(FList[Index]);
end;

function TLogListeners.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TLogListeners.Add(Listener: TLogListener);
begin
  FList.Add(Listener);
end;

procedure TLogListeners.Remove(Listener: TLogListener);
begin
  FList.Remove(Listener);
end;

{ TLogListener }

constructor TLogListener.Create;
begin
  inherited Create;
  FEnabled := True;
  FTypeFilter := DEFAULT_LOG_MESSAGE_TYPES;
  LogListeners.Add(Self);
end;

destructor TLogListener.Destroy;
begin
  LogListeners.Remove(Self);
  inherited Destroy;
end;

procedure TLogListener.Message(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Dispatcher,MessageType,Message);
end;

function TLogListener.FilterMessage(Dispatcher: TLogDispatcher; MessageType: TLogMessageType): Boolean;
begin
  Result := Enabled and (MessageType in FTypeFilter) and ((FNameFilter = '') or (FNameFilter = Dispatcher.Name));
end;

procedure TLogListener.Receive(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
begin
  if FilterMessage(Dispatcher,MessageType) then
    Self.Message(Dispatcher,MessageType,Message);
end;

{ TLogFileListener }

constructor TLogFileListener.Create(AFilename: String; Append: Boolean);
begin
  inherited Create;
  FFilename := AFilename;
  System.Assign(FFile,AFilename);
  if Append and FileExists(AFilename) then
    System.Append(FFile)
  else
    System.Rewrite(FFile);
end;

destructor TLogFileListener.Destroy;
begin
  System.Close(FFile);
  inherited Destroy;
end;

procedure TLogFileListener.Message(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
begin
  inherited Message(Dispatcher, MessageType, Message);
  Writeln(FFile,LogMessageString(Dispatcher,MessageType,Message));
  System.Flush(FFile);
end;

{ TLogCRTListener }

procedure TLogCRTListener.Message(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
begin
  case MessageType of
    mtInfo  : TextColor(LightCyan);
    mtDebug : TextColor(LightGray);
    mtWarning: TextColor(Yellow);
    mtError : TextColor(Red);
  end;
  Writeln(LogMessageString(Dispatcher,MessageType,Message));
  TextColor(LightGray);
  inherited Message(Dispatcher, MessageType, Message);
end;

{ TLogStringsListener }

procedure TLogStringsListener.Message(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
begin
  if Strings <> nil then
    FStrings.Add(LogMessageString(Dispatcher,MessageType,Message));
  inherited Message(Dispatcher, MessageType, Message);
end;

constructor TLogStringsListener.Create(Strings: TStrings);
begin
  inherited Create;
  if Assigned(Strings) then
    FStrings := Strings
  else
    FStrings := nil;
end;

initialization
  LogListeners := TLogListeners.Create;
finalization
  LogListeners.Destroy;
end.

