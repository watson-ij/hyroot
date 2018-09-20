(import [ROOT [TFile TTree TH1F TH2F TChain TBranch TCanvas THStack TLegend
               gROOT gSystem gStyle gPad
               kRed kBlue kGreen kMagenta kYellow kBlack kOrange]])
(import ROOT)
(import os)

(.SetOptStat gStyle 0)
(.SetBatch gROOT True)

(setv cvs (TCanvas))
(.cd cvs)

(defn ensure-direcs [filename]
  (setv direc (os.path.dirname filename))
  (unless (os.path.exists direc)
    (os.makedirs direc)))

(defmacro plot [filename &rest body]
  `(do (ensure-direcs ~filename)
       ~@body
       (if (in "logy" ~filename) (.SetLogy cvs True))
       (.Print gPad ~filename)
       (.SetLogy cvs False)
       (os.path.realpath ~filename)))

(defmain [&rest args]
  (setv f (TFile "test.root" "recreate")))

(defn tchain [treename &rest files &kwonly color]
  (setv out (TChain treename))
  (for [f files] (.Add out f))
  (.SetLineWidth out 2)
  (if color (.SetLineColor out color))
  out)

(defn branches [t]
  (lfor b (.GetListOfBranches t) (.GetName b)))

(deftag l [obj] `(dir ~obj))

(deftag ? [obj] `(help ~obj))

(import uuid)

(defn defhist1 [&optional [nbins 10] [xlo 0] [xhi 1] [title ""] [x ""] [y ""] [c kBlack] [lgd ""]]
  (setv name (str (uuid.uuid1)))
  (setv hist (TH1F name (+ title ";" x ";" y) nbins xlo xhi))
  (doto hist (.SetLineWidth 2) (.SetLineColor c) (setattr "lgd" lgd))
  hist)

(defn fill [ch obj &optional hist [cut ""] [opt ""]]
  (if (is None hist)
      (setv hist "")
      (setv hist (+ ">>" (.GetName hist))))
  (.Fill ch (+ obj hist) cut opt))

(defn draw [ch obj &optional [cuts ""] [opt ""] title x y ret-hist color [width 2] fit]
  (.cd cvs)
  (setv n (.Draw ch obj cuts opt))
  (setv h (.FindObject cvs "htemp"))
  (unless (is None title) (.SetTitle h title))
  (unless (is None x) (-> (.GetXaxis h) (.SetTitle x)))
  (unless (is None y) (-> (.GetYaxis h) (.SetTitle y)))
  (unless (is None color) (.SetLineColor h color))
  (unless (is None width) (.SetLineWidth h width))
  (unless (is None fit) (.Fit h fit))
  (.Update cvs)
  (if (is None ret-hist)
      n
      (do
        (setv h (.Clone h))
        (.SetName h (str (uuid.uuid1)))
        (.SetDirectory h 0)
        h)))

(defn legend [hists &optional [size [.6 .6 .9 .9]]]
  (setv l (TLegend #* size))
  (lfor h hists (.AddEntry l h (.GetTitle h)))
  (doto l (.SetLineWidth 0) (.SetFillStyle 0))
  l)

(defn stack [&optional [hists []] title x y recolor]
  (setv hh (THStack))
  (lfor h hists (.Add hh h))
  (unless (is None title) (.SetTitle hh title))
  (.Draw hh)
  (unless (is None x) (-> (.GetXaxis hh) (.SetTitle x)))
  (unless (is None y) (-> (.GetYaxis hh) (.SetTitle y)))
  (unless (is None recolor)
    (lfor i (range (len hists)) (.SetLineColor (. hists[i]) (+ i 1))))
  hh)
