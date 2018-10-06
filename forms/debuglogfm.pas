unit DebugLogFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Logging;

type
  TDebugMessage = record
    MessageType: TLogMessageType;
    Module: String;
    Message: String;
  end;
  PDebugMessage = ^TDebugMessage;

  { TDebugLogForm }

  TDebugLogForm = class(TForm)
    CBFiltered: TCheckBox;
    CBEnabled: TCheckBox;
    ClearBtn: TButton;
    CloseBtn: TButton;
    FilterText: TEdit;
    Grid: TStringGrid;
    procedure CBEnabledChange(Sender: TObject);
    procedure CBFilteredChange(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FilterTextExit(Sender: TObject);
  private
    FListener : TLogListener;
    FRecords  : TList;
    procedure ClearRecords;
    procedure HandleMessage(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
    function PassesFilter(Filter: String; Rec: PDebugMessage): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure Filter(Filter: String);
  end;

var
  DebugLogForm: TDebugLogForm;
  C: Integer = 0;

implementation

{$R *.lfm}

{ TDebugLogForm }

constructor TDebugLogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecords := TList.Create;
  FListener := TLogListener.Create;
  FListener.OnMessage := @HandleMessage;
end;

destructor TDebugLogForm.Destroy;
begin
  FListener.Destroy;
  ClearRecords;
  FRecords.Destroy;
  inherited Destroy;
end;

procedure TDebugLogForm.ClearRecords;
var
  I: Integer;
  P: PDebugMessage;
begin
  for I := 0 to FRecords.Count - 1 do
    begin
      P := FRecords[I];
      Dispose(P);
      dec(C);
    end;
  FRecords.Clear;
end;

procedure TDebugLogForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TDebugLogForm.FilterTextExit(Sender: TObject);
begin
  if CBFiltered.Checked then
    begin
      Grid.RowCount := 1;
      Filter(FilterText.Text);
    end;
end;

procedure TDebugLogForm.ClearBtnClick(Sender: TObject);
begin
  ClearRecords;
  Grid.RowCount := 1;
end;

procedure TDebugLogForm.HandleMessage(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
var
  P: PDebugMessage;
  LKind: String;
  LModule: String;
  LMessage: String;
begin
  New(P);
  inc(C);
  P^.MessageType := MessageType;
  if Assigned(Dispatcher) then
    P^.Module := Dispatcher.Name
  else
    P^.Module := '';
  P^.Message := Message;
  FRecords.Add(P);
  if (not CBFiltered.Checked) or PassesFilter(FilterText.Text,P) then
    begin
      LKind := MESSAGE_TYPE_STRINGS[P^.MessageType];
      LModule := P^.Module;
      LMessage := P^.Message;
      Grid.InsertColRow(False,Grid.RowCount);
      Grid.Cells[0,Grid.RowCount - 1] := LKind;
      Grid.Cells[1,Grid.RowCount - 1] := LModule;
      Grid.Cells[2,Grid.RowCount - 1] := LMessage;
      Show;
    end;
end;

procedure TDebugLogForm.CBFilteredChange(Sender: TObject);
begin
  if CBFiltered.Checked then
    Filter(FilterText.Text)
  else
    Filter('');
end;

procedure TDebugLogForm.CBEnabledChange(Sender: TObject);
begin
  FListener.Enabled := CBEnabled.Checked;
end;

function TDebugLogForm.PassesFilter(Filter: String; Rec: PDebugMessage): Boolean;
begin
  if Filter = '' then
    Result := True
  else
    if Filter[1] = '-' then
      Result := (Pos(Copy(Filter, 2, Length(Filter) - 1), Rec^.Module) = 0) and
                (Pos(Copy(Filter, 2, Length(Filter) - 1), Rec^.Message) = 0)
    else
      Result := (Pos(Filter, Rec^.Module) > 0) or (Pos(Filter, Rec^.Module) > 0);
end;

procedure TDebugLogForm.Filter(Filter: string);
var
  I: Integer;
  P: PDebugMessage;
begin
  Grid.RowCount := 1;
  for I := 0 to FRecords.Count - 1 do
    begin
      P := FRecords[I];
      if PassesFilter(Filter, P) then
        Grid.InsertRowWithValues(Grid.RowCount,[MESSAGE_TYPE_STRINGS[P^.MessageType],P^.Module,P^.Message]);
    end;
end;

end.
