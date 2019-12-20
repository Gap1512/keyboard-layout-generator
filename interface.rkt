#lang racket

(require racket/gui plot "source-analysis.rkt" "keyboard-render.rkt" "genetic-algorithm.rkt" "utilities.rkt")

(provide create-gui)

(define (create-gui)
  (let* ((files '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\000-Default.txt"))
         (source-material (list (multiple-files-frequencies files) (multiple-files-frequencies files #f 2)))
         (keyboard QWERTY)
         (y-by-gen '((0 0)))
         (med-by-gen '((0 0)))
         (evaluation-init '("I1: Tapping workload distribution"
                            "I2: Hand alternation"
                            "I3: Finger alternation"
                            "I4: Avoidance of big steps"
                            "I5: Hit direction"
                            "I: Evaluation score"))
         (main-window (new frame%
                           [label "Keyboard Generator"]
                           [stretchable-width #t]
                           [stretchable-height #t]))
         (main-panel (new horizontal-panel%
                          [parent main-window]
                          [style '(vscroll)]
                          [min-height 600]
                          [alignment '(left top)]))
         (setup-panel (new vertical-panel%
                           [parent main-panel]
                           [alignment '(center top)]
                           [stretchable-width #f]))
         (result-panel (new vertical-panel%
                            [parent main-panel]
                            [alignment '(center center)]
                            [min-width 800]
                            [min-height 200]))
					;[stretchable-height #t]))
         (keyboard-canvas (new canvas%
                               [parent result-panel]
					;[stretchable-height #t]
                               [paint-callback
                                (lambda (canvas dc)
                                  (send dc clear)
                                  (let* ((w (send result-panel get-width))
                                         (h (- (floor (/ (send result-panel get-height) 2)) 36)))
                                    (send dc draw-bitmap (image->bitmap (generate-keyboard keyboard (sub1 w) (sub1 h)) w h (make-bitmap w h)) 0 0)))]))
         (performance-panel (new horizontal-panel%
                                 [parent result-panel]))
					;[stretchable-height #t]))
         (performance-canvas (new canvas%
                                  [parent performance-panel]
                                  [paint-callback
                                   (lambda (canvas dc)
                                     (send dc clear)
                                     (plot-performance canvas y-by-gen med-by-gen))]))
					;[stretchable-height #t]))
         (evaluation-list (new list-box%
			       [label #f]
			       [parent performance-panel]
			       [columns '("Indicators" "Comparison" "Optimal" "Improvement")]
			       [choices evaluation-init]
			       [style (list 'single 'column-headers 'clickable-headers)]))
         (source-group (new group-box-panel%
                            [parent setup-panel]
                            [label "Source Files"]
                            [alignment '(center top)]))
         (convert (new button% 
                       [label "Convert .pdf to .txt"]
                       [parent source-group]
					;[stretchable-width #t]
                       [callback (λ (b e)
                                   (let* ((files-list (get-file-list))
                                          (frame (new frame% [label "Progress"]))
                                          (gauge (new gauge% [label #f] [parent frame] [range (length (or files-list '()))])))
                                     (send frame show #t)
                                     (send gauge set-value 0)
                                     (multiple-files-pdf->txt (map path->string files-list) gauge)
                                     (send frame show #f)))]))
         (file-list (new list-box%
                         [label #f]
                         [parent source-group]
                         [choices '()]))
         (search (new button% 
                      [label "Search"]
                      [parent source-group]
					;[stretchable-width #t]
                      [callback (λ (b e) (begin
                                           (set! files (get-file-list))
                                           (send file-list set (map path->string files))))]))
         (char-list (new list-box%
                         [label #f]
                         [parent source-group]
                         [choices '()]
                         [style (list 'single 'column-headers 'clickable-headers)]
                         [columns '("Chars" "Freqs")]))
         (analyze (new button% 
                       [label "Analyze"]
                       [parent source-group]
					;[stretchable-width #t]
                       [callback (λ (b e)
                                   (let* ((frame (new frame% [label "Progress"]))
                                          (gauge (new gauge% [label #f] [parent frame] [range (length (or files '()))])))
                                     (send frame show #t)
                                     (send gauge set-value 0)
                                     (set! source-material (multiple-files-frequencies-op files gauge))
                                     (send/apply char-list set (format-source (sort (first source-material) < #:key second)))
                                     (send frame show #f)))]))
         (weight-group (new group-box-panel%
                            [parent setup-panel]
                            [label "Weight Coefficients"]
                            [alignment '(center top)]))
         (c-i1 (new text-field%
                    [label "I1: Tapping workload distribution"]
                    [parent weight-group]
                    [init-value "0.45"]
                    [style (list 'single 'vertical-label)]))
         (c-i2 (new text-field%
                    [label "I2: Hand alternation"]
                    [parent weight-group]
                    [init-value "1.0"]
                    [style (list 'single 'vertical-label)]))
         (c-i3 (new text-field%
                    [label "I3: Finger alternation"]
                    [parent weight-group]
                    [init-value "0.8"]
                    [style (list 'single 'vertical-label)]))
         (c-i4 (new text-field%
                    [label "I4: Avoidance of big steps"]
                    [parent weight-group]
                    [init-value "0.7"]
                    [style (list 'single 'vertical-label)]))
         (c-i5 (new text-field%
                    [label "I5: Hit direction"]
                    [parent weight-group]
                    [init-value "0.6"]
                    [style (list 'single 'vertical-label)]))
         (setup-group (new group-box-panel%
                           [parent setup-panel]
                           [label "Genetic Algorithm"]
                           [alignment '(center top)]))
         (tk (new text-field%
                  [label "Tournament"]
                  [parent setup-group]
                  [init-value "4"]
                  [style (list 'single 'vertical-label)]))
         (el (new text-field%
                  [label "Elitism"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "20"]))
         (pc (new text-field%
                  [label "Crossover"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "0.6"]))
         (pm (new text-field%
                  [label "Mutation"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "0.01"]))
         (lg (new text-field%
                  [label "Last Generation"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "70"]))
         (ni (new text-field%
                  [label "Number of Individuals"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "500"]))
         (nr (new text-field%
                  [label "Number of Repetitions"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "500"]))
         (sd (new text-field%
                  [label "Standard Deviation Penalty"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "1"]))
         (ck (new text-field%
                  [label "Comparison Keyboard"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "QWERTY"]))
         (run (new button% 
                   [label "Run"]
                   [parent setup-group]
					;[stretchable-width #t]
                   [callback (λ (b e)
                               (let* ((tk (get-value tk))
                                      (el (get-value el))
                                      (pc (get-value pc))
                                      (pm (get-value pm))
                                      (lg (get-value lg))
                                      (ni (get-value ni))
                                      (nr (get-value nr))
                                      (sd (get-value sd))
                                      (ck (get-value ck))
                                      (c-i1 (get-value c-i1))
                                      (c-i2 (get-value c-i2))
                                      (c-i3 (get-value c-i3))
                                      (c-i4 (get-value c-i4))
                                      (c-i5 (get-value c-i5))
                                      (frame (new frame% [label "Progress"]))
                                      (partial-gauge (new gauge% [label #f] [parent frame] [range lg]))
                                      (total-gauge (new gauge% [label #f] [parent frame] [range (add1 nr)])))
                                 (send frame show #t)
                                 (send partial-gauge set-value 0)
                                 (send total-gauge set-value 0)
                                 (let-values ([(best best-for-generation med-for-generation scores)
                                               (time (multiple-genetic-algorithms source-material total-gauge partial-gauge
                                                                                  (list c-i1 c-i2 c-i3 c-i4 c-i5)
                                                                                  #:tournament-k tk
                                                                                  #:elitism el
                                                                                  #:pc pc
                                                                                  #:pm pm
                                                                                  #:last-generation lg
                                                                                  #:number-of-individuals ni
                                                                                  #:number-of-repetitions nr
                                                                                  #:standard-deviation-penalty sd
                                                                                  #:comparison-keyboard ck))])
                                   (set! keyboard best)
                                   (set! y-by-gen best-for-generation)
                                   (set! med-by-gen med-for-generation)
                                   (send keyboard-canvas refresh-now)
                                   (send/apply evaluation-list set (cons evaluation-init scores))
                                   (send frame show #f))))]))
         (export-group (new group-box-panel%
                            [parent setup-panel]
                            [label "Export Keyboard"]
                            [alignment '(center top)]))
         (export-width (new text-field%
                            [label "Width"]
                            [parent export-group]
                            [style (list 'single 'vertical-label)]
                            [init-value "1920"]))
         (export-height (new text-field%
                             [label "Height"]
                             [parent export-group]
                             [style (list 'single 'vertical-label)]
                             [init-value "686"]))
         (export-foreground (new text-field%
                                 [label "Foreground"]
                                 [parent export-group]
                                 [style (list 'single 'vertical-label)]
                                 [init-value "Black"]))
         (export-background (new text-field%
                                 [label "Background"]
                                 [parent export-group]
                                 [style (list 'single 'vertical-label)]
                                 [init-value "White"]))
         (export-button (new button% 
                             [label "Export"]
                             [parent export-group]
                             [stretchable-width #t]
                             [callback (λ (b e) (save-image (generate-keyboard keyboard
                                                                               (get-value export-width)
                                                                               (get-value export-height)
                                                                               #:foreground (get-string export-foreground)
                                                                               #:background (get-string export-background))
                                                            (path->string (path-replace-extension (get-file) #".png"))))])))
    (send main-window show #t)))

(define (get-value widget)
  (eval (read (open-input-string (send widget get-value)))))

(define (get-string widget)
  (string-downcase (send widget get-value)))

(define (plot-performance performance-canvas y-by-gen med-by-gen)
  (define dc (send performance-canvas get-dc))
  (send dc clear)
  (plot/dc (list (lines y-by-gen #:color 'red #:label "Best")
                 (lines med-by-gen #:color 'blue #:label "Average"))
           dc
           0 0
           (send performance-canvas get-width)
           (send performance-canvas get-height)
           #:title "Performance"
           #:x-label "Generation"
           #:y-label "Score"))
