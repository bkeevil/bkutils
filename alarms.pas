unit alarms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustomTimer;

type
  TAlarmEvent = procedure of object;

  TAlarm = class(TObject)
    private
      FEnabled    : Boolean;
      FRunOnce    : Boolean;
      FOnExecute  : TAlarmEvent;
    public
      property Enabled: Boolean read FEnabled write FEnabled;
      property RunOnce: Boolean read FRunOnce write FRunOnce default False;
      property OnExecute: TAlarmEvent read FOnExecute write FOnExecute;
  end;

  { TIntervalAlarm }

  TIntervalAlarm = class(TAlarm)
    private
      FInterval: Cardinal;
      FNext: Cardinal;
    public
      constructor Create(Interval: Cardinal; AOnExecute: TAlarmEvent = nil);
      destructor Destroy; override;
  end;

  { TTimeAlarm }

  TTimeAlarm = class(TAlarm)
    private
      FHour, FMinute: Word;
      FExecuted: Boolean;
    public
      constructor Create(Hour: Word; Minute: Word; AOnExecute: TAlarmEvent = nil);
      destructor Destroy; override;
  end;

  { TAlarmManager }

  TAlarmManager = class(TObject)
    private
      FIntervalAlarms: TList;
      FTimeAlarms: TList;
      FTimer: TCustomTimer;
      FActive: Boolean;
      FMode: Byte;
      FInterval: Int64;
      procedure CheckIntervalAlarms;
      procedure CheckTimeAlarms;
      procedure HandleTimer(Sender: TObject);
      procedure Clear;
    public
      constructor Create;
      destructor Destroy; override;
      //
      property Active: Boolean read FActive write FActive;
  end;

var
  AlarmManager: TAlarmManager;

implementation

{ TIntervalAlarm }

constructor TIntervalAlarm.Create(Interval: Cardinal; AOnExecute: TAlarmEvent);
begin
  FInterval := Interval;
  FNext := AlarmManager.FInterval + FInterval;
  FEnabled := True;
  FOnExecute := AOnExecute;
  AlarmManager.FIntervalAlarms.Add(Self);
end;

destructor TIntervalAlarm.Destroy;
begin
  FEnabled := False;
  AlarmManager.FIntervalAlarms.Remove(Self);
  inherited Destroy;
end;

{ TTimeAlarm }

constructor TTimeAlarm.Create(Hour: Word; Minute: Word; AOnExecute: TAlarmEvent);
begin
  FEnabled := True;
  FHour := Hour;
  FMinute := Minute;
  FOnExecute := AOnExecute;
  AlarmManager.FTimeAlarms.Add(Self);
end;

destructor TTimeAlarm.Destroy;
begin
  FEnabled := False;
  if Assigned(AlarmManager) then
    AlarmManager.FTimeAlarms.Remove(Self);
  inherited Destroy;
end;

{ TAlarmManager }

constructor TAlarmManager.Create;
begin
  FIntervalAlarms := TList.Create;
  FTimeAlarms := TList.Create;
  FTimer := TCustomTimer.Create(nil);
  FTimer.Interval := 500;
  FTimer.OnTimer := @HandleTimer;
  FTimer.Enabled := True;
  FMode := 0;
  FInterval := 0;
  FActive :=False;
end;

destructor TAlarmManager.Destroy;
begin
  Clear;
  FActive := False;
  FTimer.Enabled := False;
  FTimer.Free;
  FIntervalAlarms.Free;
  FTimeAlarms.Free;
  inherited Destroy;
end;

procedure TAlarmManager.Clear;
var
  X: Integer;
  O: TObject;
begin
  for X := FTimeAlarms.Count - 1 downto 0 do
    begin
      O := TObject(FTimeAlarms[X]);
      if Assigned(O) then
        O.Free;
    end;
  FTimeAlarms.Clear;
  for X := FIntervalAlarms.Count - 1 downto 0 do
    begin
      O := TObject(FIntervalAlarms[X]);
      if Assigned(O) then
        O.Free;
    end;
  FIntervalAlarms.Clear;
end;

procedure TAlarmManager.CheckIntervalAlarms;
var
  X: Integer;
  A: TIntervalAlarm;
begin
  FMode := 1;
  for X := FIntervalAlarms.Count - 1 downto 0 do
    begin
      A := TIntervalAlarm(FIntervalAlarms[X]);
      if Assigned(A) then
        begin
          if (A.Enabled) and (A.FNext <= FInterval) then
            begin
              try
                if Assigned(A.OnExecute) then
                  A.OnExecute;
              finally
                if A.RunOnce then
                  A.Free
                else
                  A.FNext := A.FNext + A.FInterval
              end;
            end;
        end;
    end;
  inc(FInterval);
end;

procedure TAlarmManager.CheckTimeAlarms;
var
  X: Integer;
  A: TTimeAlarm;
  Hour,Minute,Second,MilliSecond: Word;
begin
  FMode := 0;
  if FTimeAlarms.Count = 0 then Exit;
  DecodeTime(Now(),Hour,Minute,Second,Millisecond);
  for X := FTimeAlarms.Count - 1 downto 0 do
    begin
      A := TTimeAlarm(FTimeAlarms[X]);
      if Assigned(A) then
        begin
          if (A.Enabled) and (A.FHour = Hour) and (A.FMinute = Minute) then
            begin
              if (not A.FExecuted) then
                begin
                  A.FExecuted := True;
                  try
                    if Assigned(A.OnExecute) then
                      A.OnExecute;
                  finally
                    if A.RunOnce then
                      A.Free;
                  end;
                end;
            end
          else
            A.FExecuted := False;
        end;
    end;
end;

procedure TAlarmManager.HandleTimer(Sender: TObject);
begin
  if FActive then
    case FMode of
      0: CheckIntervalAlarms;
      1: CheckTimeAlarms;
    end;
end;

initialization
  AlarmManager := TAlarmManager.Create;
finalization
  AlarmManager.Destroy;
end.

