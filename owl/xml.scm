(define-library (owl xml)

  (import
    (owl base))

  (export
    xml-render)

  (begin

    ;; placeholders, fixme

    ;; string withing exml, tag quoted
    (define render-quoted render)

    ;; value within string, " quoted
    (define render-string-quoted render)

    ;; X = (ns? tag ((ns? attr value) ...)? . X*)
    ;;   | number
    ;;   | string
    ;;   | #false = standalone tag sans close

    (define (get-tag exp)
      (let loop ((exp exp) (tag #false))
        (cond
          ((not (pair? exp))
            (error "xml render: no tag: " exp))
          ((symbol? (car exp))
            (loop (cdr exp) (if tag (str tag ":" (car exp)) (str (car exp)))))
          (tag (values tag exp))
          (else
            (error "xml render: no tag: " exp)))))
    
    (define (get-atts exp)
      (if (and (pair? exp) (pair? (car exp)) (pair? (caar exp)))
        (values (car exp) (cdr exp))
        (values null exp)))

    (define (xren-tag tag open? tl)
      (let ((tl (render tag (if open? tl (cons #\> tl)))))
        (if open?
          (cons #\< tl)
          (ilist #\< #\/ tl))))
       
    (define (xren-atts atts tl)
      (foldr
        (λ (att tl)
          (lets ((att-name rest (get-tag att)))
            (if (and att-name (= (length rest) 1))
              (cons #\space
                (render att-name
                  (ilist #\= #\"
                    (render-string-quoted (car rest)
                      (cons #\" tl)))))
              (error "bad attribute: " att))))
        tl atts))
        
    (define (xren exp tl)
      (cond
        ((null? exp)
          tl)
        ((string? exp)
          (render-quoted exp tl))
        ((number? exp)
          (render-quoted exp tl))
        ((list? exp)
          (lets
            ((tag exp (get-tag exp))
             (atts exp (get-atts exp)))
            (xren-tag tag #true
              (xren-atts atts
                (cons #\> 
                  (foldr
                    (λ (exp tl)
                      (xren exp tl))
                    (xren-tag tag #false tl)
                    exp))))))
        (else
          (error "xml render: bad node " exp))))

    (define (xml-render exp . args)
      (list->string
        (append
          (string->list "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
          (xren exp null))))


    '(print

      (xml-render 
        `(gml featureMember (ms polygon ((fid 3)) 42))))

   '(print

      (xml-render 

        `(gml feature
          ((gml isPoint "true")
           (photocollection tag "ihq"))
          42 
          (kml structure 
            "yes"))))


    '(print)

   
    (define (render-number accuracy)
      (λ (n tl)
        (cond
          ((integer? n)
            (render n (cons #\. (fold (λ (tl n) (cons #\0 tl)) tl (iota 0 1 accuracy)))))
          (else
            (error "wat " n)))))

    ;; ((x . y) ...) → "y,x y,x ..."
    (define (gml-coords coords)
      (str coords))

    (define (polygon fid srs-name name coords)
      `(ms polygon ((fid ,fid)) 
        (gml boundedBy 
          (gml Box ((srsName ,srs-name)) 
            (gml coordinates "1.511919,47.088176 3.002191,47.882988")))
        (ms msGeometry 
          (gml Polygon ((srsName ,srs-name)) 
            (gml outerBoundaryIs 
              (gml LinearRing 
                (gml coordinates 
                  ,(gml-coords coords)))))) 
        (ms ogc_fid ,fid) 
        (ms name ,name) 
        (ms id 0)))

    (print
      (xml-render
        `(wfs FeatureCollection 
          ((xmlns ms "http://mapserver.gis.umn.edu/mapserver" )
           (xmlns wfs "http://www.opengis.net/wfs")
           (xmlns gml "http://www.opengis.net/gml")
           (xmlns ogc "http://www.opengis.net/ogc")
           (xmlns xsi "http://www.w3.org/2001/XMLSchema-instance")
           (xsi schemaLocation "http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.0.0/WFS-basic.xsd http://mapserver.gis.umn.edu/mapserver http://aneto.oco/cgi-bin/worldwfs?SERVICE=WFS&amp;VERSION=1.0.0&amp;REQUEST=DescribeFeatureType&amp;TYPENAME=polygon&amp;OUTPUTFORMAT=XMLSCHEMA"))
          (gml boundedBy
            (gml Box ((srsName "EPSG:4326"))
              (gml coordinates "-0.768746,47.003018 3.002191,47.925567")))

          (gml featureMember (ms polygon ((fid "1")) (gml boundedBy (gml Box ((srsName "EPSG:4326")) (gml coordinates "-0.768746,47.003018 0.532597,47.925567"))) (ms msGeometry (gml MultiPolygon ((srsName "EPSG:4326")) (gml polygonMember (gml Polygon (gml outerBoundaryIs (gml LinearRing (gml coordinates "-0.318987,47.003018 -0.768746,47.358268 -0.574463,47.684285 -0.347374,47.854602 -0.006740,47.925567 0.135191,47.726864 0.149384,47.599127 0.419052,47.670092 0.532597,47.428810 0.305508,47.443003 0.475824,47.144948 0.064225,47.201721 -0.318987,47.003018"))) (gml innerBoundaryIs (gml LinearRing (gml coordinates "-0.035126,47.485582 -0.035126,47.485582 -0.049319,47.641706 -0.233829,47.655899 -0.375760,47.457196 -0.276408,47.286879 -0.035126,47.485582"))))))) (ms ogc_fid 1) (ms name "My Polygon with hole") (ms id "0")))

          (gml featureMember (ms polygon ((fid "2")) (gml boundedBy (gml Box ((srsName "EPSG:4326")) (gml coordinates "1.511919,47.088176 3.002191,47.882988"))) (ms msGeometry (gml Polygon ((srsName "EPSG:4326")) (gml outerBoundaryIs (gml LinearRing (gml coordinates "1.625463,47.357844 1.511919,47.741057 1.880938,47.882988 2.420275,47.797830 2.789295,47.485582 3.002191,47.457196 2.874453,47.088176 2.178993,47.343651 1.625463,47.357844"))))) (ms ogc_fid 2) (ms name "My simple Polygon") (ms id 0)))

          (gml featureMember 
            (ms polygon ((fid "1")) 
              (gml boundedBy 
                (gml Box ((srsName "EPSG:4326")) 
                  (gml coordinates "0.000000,45.000000 2.000000,47.000000")))
              (ms msGeometry 
                (gml MultiPolygon ((srsName "EPSG:4326")) 
                  (gml polygonMember 
                    (gml Polygon 
                      (gml outerBoundaryIs (gml LinearRing (gml coordinates "0.000000,45.000000 2.000000,45.000000 2.000000,47.000000 0.000000,47.000000 0.000000,45.000000"))) 
                      (gml innerBoundaryIs (gml LinearRing (gml coordinates "0.500000,45.500000 1.500000,45.500000 1.500000,46.500000 0.500000,46.500000 0.500000,45.500000")))))))
              (ms ogc_fid 3) 
              (ms name "my polygon with hole") 
              (ms id 3)))

          )))))










  


