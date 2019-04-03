unit whatsmyip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPSend, Logging, Alarms;

{ TWhatsMyIP }

const
  DEFAULT_IP_CHECK_INTERVAL = 30;

type
  TProcessIPResponseEvent = procedure (Sender: TObject; var IP: String) of object;

  TWhatsMyIP = class(TComponent)
    private
      FEnabled: Boolean;
      FInterval: Integer;
      FURL: String;
      FIP: String;
      HTTP: THTTPSend;
      Alarm: TIntervalAlarm;
      Log: TLogDispatcher;
      FOnProcessResponse: TProcessIPResponseEvent;
      FOnChange: TNotifyEvent;
      procedure SetEnabled(AValue: Boolean);
      procedure SetInterval(AValue: Integer);
      procedure SetIP(AValue: String);
    protected
      procedure ProcessIPResponse(var IP: String); virtual;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Poll;
    published
      property Enabled: Boolean read FEnabled write SetEnabled;
      property URL: String read FURL write FURL;
      property IP: String read FIP write SetIP;
      property CheckInterval: Integer read FInterval write SetInterval default DEFAULT_IP_CHECK_INTERVAL;
      property OnProcessResponse: TProcessIPResponseEvent read FOnProcessResponse write FOnProcessResponse;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TWhatsMyIP }

constructor TWhatsMyIP.Create(AOwner: TComponent);
begin
  inherited;
  Log := TLogDispatcher.Create('WhatsMyIP');
  FURL := 'http://api.ipify.org/';
  HTTP := THTTPSend.Create;
  FEnabled := True;
  FInterval := DEFAULT_IP_CHECK_INTERVAL;
  TIntervalAlarm.Create(2,@Poll).RunOnce := True;
  Alarm := TIntervalAlarm.Create(FInterval,@Poll);
end;

destructor TWhatsMyIP.Destroy;
begin
  FreeAndNil(Alarm);
  FreeAndNil(HTTP);
  FreeAndNil(Log);
  inherited Destroy;
end;

procedure TWhatsMyIP.Poll;
var
  I: Integer;
  C: Char = #0;
  Str: String = '';
begin
  if not Enabled then Exit;
  HTTP.Document.Clear;
  if HTTP.HTTPMethod('GET',FURL) then
    begin
      for I := 1 to HTTP.Document.Size do
        begin
          HTTP.Document.Read(C,1);
          Str := Str + C;
        end;
      ProcessIPResponse(Str);
      IP := Str;
    end
  else
    Log.Send(mtWarning,'Could not fetch Public IP from "%s"',[FURL]);
end;

procedure TWhatsMyIP.ProcessIPResponse(var IP: String);
begin
  if Assigned(FOnProcessResponse) then
    FOnProcessResponse(Self,IP);
end;

procedure TWhatsMyIP.SetIP(AValue: String);
begin
  if CompareStr(AValue,FIP) <> 0 then
    begin
      Log.Send(mtInfo,'Public IP Address Changed from "%s" to "%s"',[AValue,FIP]);
      FIP := AValue;
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
end;

procedure TWhatsMyIP.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  if FEnabled then
    Alarm.Enabled := True
  else
    Alarm.Enabled := False;
end;

procedure TWhatsMyIP.SetInterval(AValue: Integer);
begin
  if FInterval=AValue then Exit;
  FInterval:=AValue;
  FreeAndNil(Alarm);
  Alarm := TIntervalAlarm.Create(FInterval,@Poll);
end;

end.

