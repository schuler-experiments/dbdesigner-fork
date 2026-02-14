
# Notes to Myself — DBDesigner Fork Lazarus Port

## Project Overview
Porting DBDesigner Fork from Delphi 7 / Kylix 3 (CLX) to Free Pascal (FPC) and Lazarus IDE (LCL).
Total: 229 tasks across 6 phases.

## Key Files
- `port-to-lazarus.md` — Main reference guide
- `port-to-lazarus-task-list.md` — Checklist of 229 tasks

## Environment
- FPC 3.2.2+dfsg-32
- lazbuild available at /usr/bin/lazbuild
- 47 .pas files in project root

## Progress Log

### Phase 0 — Project Setup & Scaffolding
- [ ] Started Phase 0
- [ ] Created DBDesignerFork.lpr
- [ ] Created CLX shim units (12 files in clx_shims/)
- [ ] Updated DBDesigner4.inc
- [ ] Converted .xfm → .lfm
- [ ] Added {$mode delphi} to all .pas files
- [ ] Created .lpi project file
- [ ] First compilation attempt

## Key Decisions
- Using shim layer approach: CLX unit names → LCL equivalents
- Keeping bundled RegExpr.pas (Option A)
- Will disable USE_SYNEDIT and USE_IXMLDBMODELType initially to reduce scope
- Will disable USE_QTheming (Windows-only, not needed)

## Known Issues
- Qt unit has NO shim equivalent — needs manual replacement
- MIDASLIB is Windows/Delphi only — needs removal
- QTheming is Windows/Delphi only — needs removal
- EmbeddedPdfImages.pas uses Qt for image conversion
