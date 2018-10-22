unit VTEditors;

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms, VirtualTrees, LMessages, LCLType, LCLIntf, Types, Spin;

type
  TVTSpinEdit = class;

  TSpinEditLink = class(TInterfacedObject, IVTEditLink)
   private
     FEdit: TVTSpinEdit;                  // A normal custom edit control.
     procedure SetEdit(const Value: TVTSpinEdit);
   protected
     FTree: TVirtualStringTree; // A back reference to the tree calling.
     FNode: PVirtualNode;             // The node to be edited.
     FColumn: TColumnIndex;           // The column of the node.
     FAlignment: TAlignment;
     FTextBounds: TRect;              // Smallest rectangle around the text.
     FStopping: Boolean;              // Set to True when the edit link requests stopping the edit action.
   public
     constructor Create; virtual;
     destructor Destroy; override;

     function BeginEdit: Boolean; virtual; stdcall;
     function CancelEdit: Boolean; virtual; stdcall;
     property Edit: TVTSpinEdit read FEdit write SetEdit;
     function EndEdit: Boolean; virtual; stdcall;
     function GetBounds: TRect; virtual; stdcall;
     function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
     procedure ProcessMessage(var Message: TLMessage); virtual; stdcall;
     procedure SetBounds(R: TRect); virtual; stdcall;
   end;

  { TVTSpinEdit }

  TVTSpinEdit = class(TCustomSpinEdit)
  private
    procedure CMAutoAdjust(var Message: TLMessage); message CM_AUTOADJUST;
    procedure CMExit(var Message: TLMessage); message CM_EXIT;
    procedure CNCommand(var Message: TLMCommand); message CN_COMMAND;
    procedure DoRelease(Data: PtrInt);
    procedure WMChar(var Message: TLMChar); message LM_CHAR;
    procedure WMDestroy(var Message: TLMDestroy); message LM_DESTROY;
    procedure WMGetDlgCode(var Message: TLMNoParams); message LM_GETDLGCODE;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
  protected
    FRefLink: IVTEditLink;
    FLink: TSpinEditLink;
    procedure AutoAdjustSize; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(Link: TSpinEditLink); reintroduce;

    procedure Release;

    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    //property HideSelection;
    property MaxLength;
    //property OEMConvert;
    property PasswordChar;
  end;

implementation

type
  TVirtualTreeHack = class(TVirtualStringTree);

constructor TSpinEditLink.Create;
begin
  inherited;
  FEdit := TVTSpinEdit.Create(Self);
  with FEdit do
  begin
    Visible := False;
    BorderStyle := bsNone;
    AutoSize := False;
  end;
end;

destructor TSpinEditLink.Destroy;
begin
  FEdit.Release;
  inherited;
end;

function TSpinEditLink.BeginEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
  begin
    FEdit.Show;
    FEdit.SelectAll;
    FEdit.SetFocus;
  end;
end;

procedure TSpinEditLink.SetEdit(const Value: TVTSpinEdit);
begin
  if Assigned(FEdit) then
    FEdit.Free;
  FEdit := Value;
end;

function TSpinEditLink.CancelEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
  begin
    FStopping := True;
    FEdit.Hide;
    FTree.CancelEditNode;
    FEdit.FLink := nil;
    FEdit.FRefLink := nil;
  end;
end;

function TSpinEditLink.EndEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;
    if FEdit.Modified then
      FTree.Text[FNode, FColumn] := FEdit.Text;
    FEdit.Hide;
    FEdit.FLink := nil;
    FEdit.FRefLink := nil;
  except
    FStopping := False;
    raise;
  end;
end;

function TSpinEditLink.GetBounds: TRect; stdcall;
begin
  Result := FEdit.BoundsRect;
end;

function TSpinEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  Text: String;
begin
  Result := Tree is TCustomVirtualStringTree;
  if Result then
  begin
    FTree := Tree as TVirtualStringTree;
    FNode := Node;
    FColumn := Column;
    // Initial size, font and text of the node.
    FTree.GetTextInfo(Node, Column, FEdit.Font, FTextBounds, Text);
    FEdit.Font.Color := clWindowText;
    FEdit.Parent := Tree;
    FEdit.HandleNeeded;
    FEdit.Text := Text;

    if Column <= NoColumn then
    begin
      FEdit.BidiMode := FTree.BidiMode;
      FAlignment := FTree.Alignment;
    end
    else
    begin
      FEdit.BidiMode := FTree.Header.Columns[Column].BidiMode;
      FAlignment := FTree.Header.Columns[Column].Alignment;
    end;

    if FEdit.BidiMode <> bdLeftToRight then
      ChangeBidiModeAlignment(FAlignment);
  end;
end;

procedure TSpinEditLink.ProcessMessage(var Message: TLMessage); stdcall;
begin
  FEdit.WindowProc(Message);
end;

procedure TSpinEditLink.SetBounds(R: TRect); stdcall;
var
  Offset: Integer;
begin
  if not FStopping then
    begin
      with R do
        begin
          // Set the edit's bounds but make sure there's a minimum width and the right border does not
          // extend beyond the parent's left/right border.
          if Left < 0 then
            Left := 0;
          if Right - Left < 30 then
          begin
            if FAlignment = taRightJustify then
              Left := Right - 30
            else
              Right := Left + 30;
          end;
          if Right > FTree.ClientWidth then
            Right := FTree.ClientWidth;
          FEdit.BoundsRect := R;

          // The selected text shall exclude the text margins and be centered vertically.
          // We have to take out the two pixel border of the edit control as well as a one pixel "edit border" the
          // control leaves around the (selected) text.
          R := FEdit.ClientRect;
          Offset := 2;
          if tsUseThemes in FTree.TreeStates then
            Inc(Offset);
          InflateRect(R, -FTree.TextMargin + Offset, Offset);
          if not (vsMultiline in FNode^.States) then
            OffsetRect(R, 0, FTextBounds.Top - FEdit.Top);

          SendMessage(FEdit.Handle, EM_SETRECTNP, 0, PtrUInt(@R));
      end;
    end;
end;

{ TVTSpinEdit }

constructor TVTSpinEdit.Create(Link: TSpinEditLink);
begin
  inherited Create(nil);
  ShowHint := False;
  ParentShowHint := False;
  // This assignment increases the reference count for the interface.
  FRefLink := Link;
  // This reference is used to access the link.
  FLink := Link;
end;

procedure TVTSpinEdit.CMAutoAdjust(var Message: TLMessage);
begin
  AutoAdjustSize;
end;

procedure TVTSpinEdit.CMExit(var Message: TLMessage);

begin
  if Assigned(FLink) and not FLink.FStopping then
    with FLink, TVirtualTreeHack(FTree) do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) then
        DoEndEdit
      else
        DoCancelEdit;
    end;
end;

procedure TVTSpinEdit.CNCommand(var Message: TLMCommand);
begin
  if Assigned(FLink) and Assigned(FLink.FTree) and (Message.NotifyCode = EN_UPDATE) and
    not (toGridExtensions in FLink.FTree.TreeOptions.MiscOptions) and
    not (vsMultiline in FLink.FNode^.States) then
    // Instead directly calling AutoAdjustSize it is necessary on Win9x/Me to decouple this notification message
    // and eventual resizing. Hence we use a message to accomplish that.
    if IsWinNT then
      AutoAdjustSize
    else
      PostMessage(Handle, CM_AUTOADJUST, 0, 0);
end;

procedure TVTSpinEdit.DoRelease(Data: PtrInt);
begin
  Free;
end;

procedure TVTSpinEdit.WMChar(var Message: TLMChar);
begin
  if not (Message.CharCode in [VK_ESCAPE, VK_TAB]) then
    inherited;
end;

procedure TVTSpinEdit.WMDestroy(var Message: TLMDestroy);
begin
  // If editing stopped by other means than accept or cancel then we have to do default processing for
  // pending changes.
  if Assigned(FLink) and not FLink.FStopping then
  begin
    with FLink, FTree do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) and Modified then
        Text[FNode, FColumn] := FEdit.Text;
    end;
    FLink := nil;
    FRefLink := nil;
  end;

  inherited;
end;

procedure TVTSpinEdit.WMGetDlgCode(var Message: TLMNoParams);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTALLKEYS or DLGC_WANTTAB or DLGC_WANTARROWS;
end;

procedure TVTSpinEdit.WMKeyDown(var Message: TLMKeyDown);
var
  Shift: TShiftState;
  EndEdit: Boolean;
  Tree: TBaseVirtualTree;
begin
  case Message.CharCode of
    VK_ESCAPE:
      begin
        Tree := FLink.FTree;
        TVirtualTreeHack(FLink.FTree).DoCancelEdit;
        Tree.SetFocus;
      end;
    VK_RETURN:
      begin
        EndEdit := not (vsMultiline in FLink.FNode^.States);
        if not EndEdit then
        begin
          // If a multiline node is being edited the finish editing only if Ctrl+Enter was pressed,
          // otherwise allow to insert line breaks into the text.
          Shift := KeyDataToShiftState(Message.KeyData);
          EndEdit := ssCtrlOS in Shift;
        end;
        if EndEdit then
        begin
          Tree := FLink.FTree;
          FLink.FTree.InvalidateNode(FLink.FNode);
          TVirtualTreeHack(FLink.FTree).DoEndEdit;
          Tree.SetFocus;
        end;
      end;
    VK_UP:
      begin
        if not (vsMultiline in FLink.FNode^.States) then
          Message.CharCode := VK_LEFT;
        inherited;
      end;
    VK_DOWN:
      begin
        if not (vsMultiline in FLink.FNode^.States) then
          Message.CharCode := VK_RIGHT;
        inherited;
      end;
  else
    inherited;
  end;
end;

procedure TVTSpinEdit.AutoAdjustSize;
// Changes the size of the edit to accomodate as much as possible of its text within its container window.
// NewChar describes the next character which will be added to the edit's text.
var
  DC: HDC;
  Size: TSize;
  LastFont: THandle;
begin
  if not (vsMultiline in FLink.FNode^.States) then
  begin
    DC := GetDC(Handle);
    LastFont := SelectObject(DC, Font.Reference.Handle);
    try
      // Read needed space for the current text.
      GetTextExtentPoint32(DC, PChar(Text), Length(Text), Size);
      Inc(Size.cx, 2 * FLink.FTree.TextMargin);

      // Repaint associated node if the edit becomes smaller.
      if Size.cx < Width then
        FLink.FTree.InvalidateNode(FLink.FNode);

      if FLink.FAlignment = taRightJustify then
        FLink.SetBounds(Rect(Left + Width - Size.cx, Top, Left + Width, Top + Height))
      else
        FLink.SetBounds(Rect(Left, Top, Left + Size.cx, Top + Height));
    finally
      SelectObject(DC, LastFont);
      ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TVTSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;

  // Only with multiline style we can use the text formatting rectangle.
  // This does not harm formatting as single line control, if we don't use word wrapping.
  with Params do
  begin
    //todo: delphi uses Multiline for all
    //Style := Style or ES_MULTILINE;
    if vsMultiline in FLink.FNode^.States then
    begin
      Style := Style and not (ES_AUTOHSCROLL or WS_HSCROLL) or WS_VSCROLL or ES_AUTOVSCROLL;
      Style := Style or ES_MULTILINE;
    end;
    if tsUseThemes in FLink.FTree.TreeStates then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end
    else
    begin
      Style := Style or WS_BORDER;
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TVTSpinEdit.Release;
begin
  if HandleAllocated then
    Application.QueueAsyncCall(@DoRelease, 0);
end;

end.
