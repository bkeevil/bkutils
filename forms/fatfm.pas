unit fatfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids, Files;

type
  TFileHack = class(TFile);
  { TFatInspectorForm }

  TFatInspectorForm = class(TForm)
    RefreshBtn: TButton;
    CloseBtn: TButton;
    Splitter1: TSplitter;
    FilePageGrid: TStringGrid;
    Splitter2: TSplitter;
    FATPageGrid: TStringGrid;
    FileListGrid: TStringGrid;
    procedure CloseBtnClick(Sender: TObject);
    procedure FATPageGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure FileListGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure RefreshBtnClick(Sender: TObject);
  private
    procedure LoadFileList(Container: TFileContainer);
    procedure LoadFATPageList(AFile: TFileHack);
  public
    FContainer: TFileContainer;
    SelectedFile: TFileHack;
    procedure LoadData(Container: TFileContainer);
    procedure ClearData;
  end;

var
  FatInspectorForm: TFatInspectorForm;

procedure ShowFATInspector(Container: TFileContainer);

implementation

uses
  FileContainerFM;

type
  TFileFATHack = class(TFileFAT);

procedure ShowFATInspector(Container: TFileContainer);
begin
  if not Assigned(FATInspectorForm) then
    FATInspectorForm := TFATInspectorForm.Create(FileContainerForm);
  FATInspectorForm.LoadData(Container);
  FATInspectorForm.Show;
end;

{$R *.lfm}

{ TFatInspectorForm }

procedure TFatInspectorForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TFatInspectorForm.FATPageGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var
  P: PFATPage;
  R,C,I: Integer;
begin
  if aRow = 0 then Exit;
  FilePageGrid.RowCount := (MAX_FAT_PAGE_ENTRIES + 7) div 8 + 1;
  P := PFATPage(TFileFATHack(SelectedFile.FAT).FATPages[aRow - 1]^.Data);
  R := 1; C := 1; I := 1;
  while I < MAX_FAT_PAGE_ENTRIES do
    begin
      FilePageGrid.Cells[C,R] := IntToStr(P^.Pages[I]);
      inc(I); inc(C);
      if C > 8 then
        begin
          inc(R);
          C := 1;
        end;
    end;
end;

procedure TFatInspectorForm.FileListGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  if aRow > 0 then
  begin
    FATPageGrid.RowCount := 1;
    SelectedFile := TFileHack(FContainer.Files[aRow - 1]);
    LoadFATPageList(TFileHack(SelectedFile));
  end;
end;

procedure TFatInspectorForm.RefreshBtnClick(Sender: TObject);
begin
  LoadData(FContainer);
end;

procedure TFatInspectorForm.LoadFileList(Container: TFileContainer);
var
  I: Integer;
  F: TFile;
begin
  FileListGrid.RowCount := Container.Files.Count + 1;
  for I := 0 to Container.Files.Count - 1 do
    begin
      F := Container.Files[I];
      FileListGrid.Cells[0,I+1] := F.Name;
    end;
end;

procedure TFatInspectorForm.LoadFATPageList(AFile: TFileHack);
var
  I: Integer;
  Page: PBufferedPage;
begin
  FATPageGrid.RowCount := TFileFATHack(AFile.FAT).FATPageCount + 1;
  for I := 0 to TFileFATHack(AFile.FAT).FATPageCount - 1 do
    begin
      Page := TFileFATHack(AFile.FAT).FATPages[I];
      FATPageGrid.Cells[0,I+1] := IntToStr(I);
      FATPageGrid.Cells[1,I+1] := IntToStr(Page^.PageNum);
      FATPageGrid.Cells[2,I+1] := IntToStr(Page^.PageIdx);
      if (Page^.Modified) then
        FATPageGrid.Cells[3,I+1] := '1'
      else
        FATPageGrid.Cells[3,I+1] := '0';
      FATPageGrid.Cells[4,I+1] := IntToStr(PFATPage(Page^.Data)^.Next);
    end;
end;

procedure TFatInspectorForm.LoadData(Container: TFileContainer);
begin
  FContainer := Container;
  ClearData;
  LoadFileList(Container);
end;

procedure TFatInspectorForm.ClearData;
begin
  FileListGrid.RowCount := 1;
  FATPageGrid.RowCount := 1;
  FilePageGrid.RowCount := 1;
end;

end.
