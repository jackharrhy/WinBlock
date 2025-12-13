unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    ExportMemo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    SelectedImage: TPicture;
    SelectedImageIndex: Integer;
    GridState: array[0..15, 0..15] of Integer;
    GridImages: array[0..15, 0..15] of TImage;
    PaletteImages: TList;
    PaletteFilenames: TStringList;
    SelectedShape: TShape;
    GridBackground: TImage;
    procedure LoadToPanel(const Dir: string);
    procedure CreateGrid;
    procedure PaletteImageClick(Sender: TObject);
    procedure GridImageClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CopyFromGridCell(Col, Row: Integer);
    procedure ShowSelectionBorder(PaletteIndex: Integer);
    procedure UpdateExportText;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.LoadToPanel(const Dir: string);
var
  SR: TSearchRec;
  Pic: TPicture;
  Img: TImage;
  Col, Row: Integer;
  ImagesPerRow: Integer;
begin
  ImagesPerRow := Panel1.Width div 32;
  Col := 0;
  Row := 0;

  if FindFirst(Dir + PathDelim + '*.*', faAnyFile, SR) = 0 then
  repeat
    if (SR.Attr and faDirectory) = 0 then
    begin
      Pic := TPicture.Create;
      try
        Pic.LoadFromFile(Dir + PathDelim + SR.Name);

        Img := TImage.Create(Panel1);
        Img.Picture.Assign(Pic);
        Img.Parent := Panel1;
        Img.Left := Col * 32;
        Img.Top := Row * 32;
        Img.Stretch := True;
        Img.Width := 32;
        Img.Height := 32;
        Img.OnClick := @PaletteImageClick;
        PaletteImages.Add(Img);
        PaletteFilenames.Add(ChangeFileExt(SR.Name, ''));

        Inc(Col);
        if Col >= ImagesPerRow then
        begin
          Col := 0;
          Inc(Row);
        end;

      except
        Pic.Free;
      end;
    end;
  until FindNext(SR) <> 0;
  FindClose(SR);
end;

procedure TMainForm.CreateGrid;
var
  Col, Row: Integer;
  CellSize: Integer;
  Img: TImage;
  GridBitmap: TBitmap;
begin
  CellSize := Panel3.Width div 16;

  // Create grid background image
  GridBackground := TImage.Create(Panel3);
  GridBackground.Parent := Panel3;
  GridBackground.Left := 0;
  GridBackground.Top := 0;
  GridBackground.Width := Panel3.Width;
  GridBackground.Height := Panel3.Height;
  GridBackground.Stretch := False;
  GridBackground.SendToBack;

  // Draw grid on background
  GridBitmap := TBitmap.Create;
  GridBitmap.SetSize(Panel3.Width, Panel3.Height);
  GridBitmap.Canvas.Brush.Color := clWhite;
  GridBitmap.Canvas.FillRect(0, 0, GridBitmap.Width, GridBitmap.Height);
  GridBitmap.Canvas.Pen.Color := clGray;
  GridBitmap.Canvas.Pen.Width := 1;

  // Vertical lines
  for Col := 0 to 16 do
  begin
    GridBitmap.Canvas.MoveTo(Col * CellSize, 0);
    GridBitmap.Canvas.LineTo(Col * CellSize, Panel3.Height);
  end;

  // Horizontal lines
  for Row := 0 to 16 do
  begin
    GridBitmap.Canvas.MoveTo(0, Row * CellSize);
    GridBitmap.Canvas.LineTo(Panel3.Width, Row * CellSize);
  end;

  GridBackground.Picture.Assign(GridBitmap);
  GridBitmap.Free;

  // Create transparent grid cells
  for Row := 0 to 15 do
    for Col := 0 to 15 do
    begin
      Img := TImage.Create(Panel3);
      Img.Parent := Panel3;
      Img.Left := Col * CellSize;
      Img.Top := Row * CellSize;
      Img.Width := CellSize;
      Img.Height := CellSize;
      Img.Stretch := True;
      Img.Picture.Bitmap := nil;
      Img.Transparent := True;

      GridImages[Col, Row] := Img;
      GridState[Col, Row] := -1;

      Img.OnMouseDown := @GridImageClick;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PaletteImages := TList.Create;
  PaletteFilenames := TStringList.Create;
  SelectedImage := nil;
  SelectedImageIndex := -1;
  SelectedShape := nil;
  LoadToPanel('.\bin');
  CreateGrid;
  UpdateExportText;
end;

procedure TMainForm.ShowSelectionBorder(PaletteIndex: Integer);
var
  PaletteImg: TImage;
begin
  if Assigned(SelectedShape) then
    SelectedShape.Free;

  if (PaletteIndex >= 0) and (PaletteIndex < PaletteImages.Count) then
  begin
    PaletteImg := TImage(PaletteImages[PaletteIndex]);

    SelectedShape := TShape.Create(Panel1);
    SelectedShape.Parent := Panel1;
    SelectedShape.Shape := stRectangle;
    SelectedShape.Brush.Style := bsClear;
    SelectedShape.Pen.Color := clBlue;
    SelectedShape.Pen.Width := 2;
    SelectedShape.Left := PaletteImg.Left - 2;
    SelectedShape.Top := PaletteImg.Top - 2;
    SelectedShape.Width := PaletteImg.Width + 4;
    SelectedShape.Height := PaletteImg.Height + 4;
    SelectedShape.BringToFront;
    PaletteImg.BringToFront;
  end;
end;

procedure TMainForm.PaletteImageClick(Sender: TObject);
var
  Img: TImage;
  I: Integer;
begin
  if Sender is TImage then
  begin
    if Assigned(SelectedImage) then
      SelectedImage.Free;

    SelectedImage := TPicture.Create;
    SelectedImage.Assign(TImage(Sender).Picture);

    for I := 0 to PaletteImages.Count - 1 do
    begin
      if TImage(PaletteImages[I]) = Sender then
      begin
        SelectedImageIndex := I;
        Break;
      end;
    end;

    ShowSelectionBorder(SelectedImageIndex);
  end;
end;

procedure TMainForm.GridImageClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
  CellSize: Integer;
  Img: TImage;
begin
  if Sender is TImage then
  begin
    Img := TImage(Sender);
    CellSize := Panel3.Width div 16;
    Col := Img.Left div CellSize;
    Row := Img.Top div CellSize;

    if (Col >= 0) and (Col <= 15) and (Row >= 0) and (Row <= 15) then
    begin
      if Button = mbLeft then
      begin
        if Assigned(SelectedImage) then
        begin
          Img.Picture.Assign(SelectedImage);
          GridState[Col, Row] := SelectedImageIndex;
          UpdateExportText;
        end;
      end
    else if Button = mbRight then
      begin
        Img.Picture.Bitmap := nil;
        GridState[Col, Row] := -1;
        UpdateExportText;
      end
      else if Button = mbMiddle then
      begin
        CopyFromGridCell(Col, Row);
      end;
    end;
  end;
end;

procedure TMainForm.CopyFromGridCell(Col, Row: Integer);
var
  CellValue: Integer;
begin
  CellValue := GridState[Col, Row];

  if CellValue >= 0 then
  begin
    // Copy the image from the grid cell to selection
    if Assigned(SelectedImage) then
      SelectedImage.Free;

    SelectedImage := TPicture.Create;
    SelectedImage.Assign(GridImages[Col, Row].Picture);
    SelectedImageIndex := CellValue;

    ShowSelectionBorder(SelectedImageIndex);
  end;
end;

procedure TMainForm.UpdateExportText;
var
  Row, Col: Integer;
  ExportText: string;
  CellValue: Integer;
  MinCol, MaxCol, MinRow, MaxRow: Integer;
  HasContent: Boolean;
begin
  // Find the bounds of the painted area
  MinCol := 15;
  MaxCol := 0;
  MinRow := 15;
  MaxRow := 0;
  HasContent := False;

  for Row := 0 to 15 do
  begin
    for Col := 0 to 15 do
    begin
      if GridState[Col, Row] >= 0 then
      begin
        HasContent := True;
        if Col < MinCol then MinCol := Col;
        if Col > MaxCol then MaxCol := Col;
        if Row < MinRow then MinRow := Row;
        if Row > MaxRow then MaxRow := Row;
      end;
    end;
  end;

  if not HasContent then
  begin
    ExportMemo.Text := 'Discord export will appear here...';
    Exit;
  end;

  ExportText := '';

  // Export only the shrinkwrapped area
  for Row := MinRow to MaxRow do
  begin
    for Col := MinCol to MaxCol do
    begin
      CellValue := GridState[Col, Row];
      if CellValue >= 0 then
      begin
        if (CellValue < PaletteFilenames.Count) then
          ExportText := ExportText + ':' + PaletteFilenames[CellValue] + ':'
        else
          ExportText := ExportText + ':unknown:';
      end
      else
      begin
        ExportText := ExportText + ':00:';
      end;
    end;
    if Row < MaxRow then
      ExportText := ExportText + #13#10;
  end;

  ExportMemo.Text := ExportText;
end;

end.

