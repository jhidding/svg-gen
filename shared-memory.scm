(define style-sheet
  "
@import url('https://fonts.googleapis.com/css2?family=Nunito:wght@700');

:root {
   --nlesc-blue:   #009DDD;
   --nlesc-purple: #380339;
   --nlesc-yellow: #FFB213;
}

text {
   font-family: 'Nunito';
   font-weight: 700;
}

.router-light {
   fill: var(--nlesc-yellow);
}

.fat-arrow {
   stroke: var(--nlesc-blue);
   stroke-width: 10;
   stroke-linecap: round;
}

.lan {
   stroke: var(--nlesc-blue);
   stroke-width: 5;
}

.group {
   stroke: var(--nlesc-blue);
   stroke-width: 5;
   fill: none;
}

.cpu text {
   fill: white;
}

text {
   fill: white;
}

.case {
    fill: var(--nlesc-purple);
    stroke: none; /*#380339;  var(nlesc-purple);*/
    border-radius: 5pt;
}

.gradientCpuBorder-0 {
   stop-color: #380339;
}

.gradientCpuBorder-1 {
   stop-color: white;
}

.connectors {
    stroke: var(--nlesc-yellow);
    stroke-width: 2pt;
    filter: drop-shadow( 0px 0px 2px rgba(0, 0, 0, .3));
}
  ")

(define (linspace a b n)
  (let ((step (/ (- b a) (- n 1))))
    (map (lambda (i) (+ a (* step i)))
         (range n))))

(define (connectors id)
   `((g id: ,id class: "connectors")
     ,@(map (lambda (i) `(line x1: ,i y1: -15 x2: ,i y2: 5 /))
            (linspace 20.0 80.0 8))
     (/g)))

(define cpu
  `((g class: "cpu")
    (rect class: "case" x: 0 y: 0 width: 100 height: 100 rx: 10 /)
    ,@(connectors "cpu-connectors")
    (use xlink:href: "#cpu-connectors" transform: "rotate(90,  50, 50)" /)
    (use xlink:href: "#cpu-connectors" transform: "rotate(180, 50, 50)" /)
    (use xlink:href: "#cpu-connectors" transform: "rotate(-90, 50, 50)" /)
    (text x: 50 y: 52 text-anchor: "middle" dominant-baseline: "middle" 
          font-size: 30) "CPU" (/text)
    (/g)))

(define ram
  `((g class: "ram")
    (rect class: "case" x: 0 y: 0 width: 100 height: 50 rx: 10 /)
    ,@(connectors "ram-connectors")
    (use xlink:href: "#ram-connectors" transform: "rotate(180, 50, 25)" /)
    (text x: 50 y: 27 text-anchor: "middle" dominant-baseline: "middle"
          font-size: 30) "RAM" (/text)
    (/g)))

(define compute-unit
  `((g)
  (rect class: "group" x: -25 y: -25 width: 150 height: 300 rx: 20 /)
  ,@cpu
  (g transform: "translate(0, 200)") ,@ram (/g)
  (line x1: 50 y1: 120 x2: 50 y2: 180 class: "fat-arrow" /)
  (/g))
)

(define router
  `((g)
    (rect class: "group" x: -25 y: -25 width: 300 height: 150 rx: 20 /)
    (rect class: "case" x: -10 y: -10 width: 270 height: 39 rx: 10 /)
    (rect class: "case" x: -10 y:  30 width: 270 height: 39 rx: 10 /)
    (rect class: "case" x: -10 y:  70 width: 270 height: 40 rx: 10 /)
    (text x: 125 y: 52 text-anchor: "middle" dominant-baseline: "middle"
          font-size: 30) "ROUTER" (/text)
    ,@(map (lambda (i) `(circle class: "router-light" cx: 5 cy: ,(+ 5 (* i 40)) r: 5 /))
          (range 3))
    (/g)))

(define (translate dx dy element)
  `((g transform: ,(format "translate({},{})" dx dy)) ,@element (/g)))

`((svg viewBox: "-30 -30 560 385"
       xmlns: "http://www.w3.org/2000/svg"
       xmlns:xlink: "http://www.w3.org/1999/xlink")
  (style) ,style-sheet (/style)
  (rect class: "group" x: -25 y: -25 width: 550 height: 375 rx: 20 /)
  ,@(append-map (lambda (i) (translate (* 200 i) 0 cpu))
                (range 3))
  (rect class: "case" x: 125 y: 200 width: 250 height: 40 rx: 10 /)
  (text x: 250 y: 222 text-anchor: "middle" dominant-baseline: "middle"
        font-size: 30) "MEMORY BUS" (/text)
  ,@(translate 125 270 ram)
  ,@(translate 275 270 ram)
  (line class: "fat-arrow" x1: 50 y1: 125 x2: 200 y2: 190 /)
  (line class: "fat-arrow" x1: 250 y1: 125 x2: 250 y2: 190 /)
  (line class: "fat-arrow" x1: 450 y1: 125 x2: 300 y2: 190 /)
  (/svg))
