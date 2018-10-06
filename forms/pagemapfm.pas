unit PageMapFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Files;

const
  BOX_SIZE    = 10;
  BOX_PADDING = 2;

type
  TFileContainerHack = class(TFileContainer);
  TFileHack = class(TFile);

  { TPageMapForm }

  TPageMapForm = class(TForm)
    PaintBox: TPaintBox;
    ScrollBox: TScrollBox;
    StatusBar: TStatusBar;
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
  private
    procedure CalculateMetrics;
    procedure ClearPaintBox;
    function GetValueColor(Value: SmallInt): TColor;
    procedure PaintCell(Col, Row: Integer);
  public
    Container: TFileContainer;
    PageMap: TPageMap;
    CanvasHeight, CanvasWidth: Integer;
    ColCount, RowCount: Integer;
    procedure RefreshMap;
  end;

var
  PageMapForm: TPageMapForm;

procedure ShowPageMap(Container: TFileContainer);

implementation

procedure ShowPageMap(Container: TFileContainer);
begin
  if not Assigned(PageMapForm) then
    PageMapForm := TPageMapForm.Create(Application);
  PageMapForm.Container := Container;
  PageMapForm.PageMap := Container.PageMap;
  Container.PageMap.Build;
  Container.PageMap.Validate;
  PageMapForm.RefreshMap;
  PageMapForm.Show;
end;

{$R *.lfm}

{ TPageMapForm }

procedure TPageMapForm.PaintBoxPaint(Sender: TObject);
var
  Col, Row: Integer;
begin
  CalculateMetrics;
  ClearPaintBox;
  for Row := 0 to RowCount - 1 do
    for Col := 0 to ColCount - 1 do
      PaintCell(Col,Row);
end;

procedure TPageMapForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  C,R,P: Integer;
  ID: Integer;
  I: Integer;
  FI: TFile;
  Filename: String;
begin
  C := (X - (BOX_PADDING div 2)) div (BOX_SIZE + BOX_PADDING);
  R := (Y - (BOX_PADDING div 2)) div (BOX_SIZE + BOX_PADDING);
  P := (ColCount * R) + C;
  Filename := '';
  if P < PageMap.Count then
    begin
      ID := PageMap.Values[P];
      if ID > 0 then
        begin
          I := Container.Files.Find(ID);
          if I >= 0 then
            begin
              FI := Container.Files[I];
              if Assigned(FI) then
                Filename := FI.Name + ' DATA';
            end;
        end
      else
        if ID = PAGEMAP_HEADER then
          Filename := 'Header'
        else
          if ID = PAGEMAP_META then
            Filename := 'Metadata'
          else
            if ID = PAGEMAP_FREE then
              Filename := 'Free'
            else
              if ID > -MAX_FILE_COUNT then
                begin
                  I := Container.Files.Find(-ID);
                  if I >= 0 then
                    begin
                      FI := Container.Files[I];
                      if Assigned(FI) then
                        Filename := FI.Name + ' FAT';
                    end;
                end;
    end;
  StatusBar.SimpleText := Format('Page %d - %s',[P,Filename]);
end;

procedure TPageMapForm.CalculateMetrics;
begin
  CanvasWidth := PaintBox.Width;
  ColCount := (CanvasWidth  - BOX_PADDING) div (BOX_SIZE + BOX_PADDING);
  if ColCount <= 0 then
    begin
      RowCount := 0;
      ColCount := 0;
      Exit;
    end
  else
    begin
      RowCount := PageMap.Count div ColCount;
      if PageMap.Count mod ColCount > 0 then inc(RowCount);
    end;
  CanvasHeight := RowCount * (BOX_SIZE + BOX_PADDING);
  CanvasWidth  := ColCount * (BOX_SIZE + BOX_PADDING);
  PaintBox.Height := CanvasHeight;
end;

procedure TPageMapForm.ClearPaintBox;
begin
  PaintBox.Canvas.Brush.Color := PaintBox.Color;
  PaintBox.Canvas.FillRect(PaintBox.ClientRect);
end;

function GetColor(Value: Byte): TColor;
var
  R,G,B: Byte;
begin
  B := (Value       and 1) or (Value shr 2 and 2);
  G := (Value shr 1 and 1) or (Value shr 3 and 2);
  R := (Value shr 2 and 1) or (Value shr 4 and 2);
  B := B * 64 + 16;
  G := G * 64 + 16;
  R := R * 64 + 16;
  Result := RGBToColor(R,G,B);
end;

function TPageMapForm.GetValueColor(Value: SmallInt): TColor;
begin
  case Value of
    PAGEMAP_HEADER : Result := clBlack;
    PAGEMAP_META   : Result := clRed;
    PAGEMAP_FREE   : Result := clBlue;
  else
    begin
      if Value < 0 then
        Value := -Value;
      Result := GetColor(Value mod 64);
    end;
  end;
end;

procedure TPageMapForm.PaintCell(Col,Row: Integer);
var
  Index: Integer;
  Value: SmallInt;
  AColor: TColor;
  Rect : TRect;
begin
  Index := (ColCount * Row) + Col;
  if Index < PageMap.Count then
    Value := PageMap.Values[Index]
  else
    Exit;
  AColor := GetValueColor(Value);
  Rect.Left := BOX_PADDING + ((BOX_SIZE + BOX_PADDING) * Col);
  Rect.Right := Rect.Left + BOX_SIZE;
  Rect.Top := BOX_PADDING + ((BOX_SIZE + BOX_PADDING) * Row);
  Rect.Bottom := Rect.Top + BOX_SIZE;
  PaintBox.Canvas.Brush.Color := AColor;
  if Value < 0 then
    begin
      PaintBox.Canvas.Pen.Color := AColor;
      PaintBox.Canvas.Pen.Width := 2;
      PaintBox.Canvas.Brush.Color := PaintBox.Color;
    end
  else
    begin
      PaintBox.Canvas.Pen.Color := AColor;
      PaintBox.Canvas.Pen.Width := 1;
      PaintBox.Canvas.Brush.Color := AColor;
    end;
  PaintBox.Canvas.FillRect(Rect);
  PaintBox.Canvas.Rectangle(Rect);
end;

procedure TPageMapForm.ScrollBoxResize(Sender: TObject);
begin
  PaintBox.Top := 0;
  PaintBox.Left := 0;
  PaintBox.Width := ScrollBox.ClientWidth;
  CalculateMetrics;
  ClearPaintBox;
end;

procedure TPageMapForm.RefreshMap;
begin
  PaintBox.Invalidate;
  PageMapForm.Show;
end;

end.

