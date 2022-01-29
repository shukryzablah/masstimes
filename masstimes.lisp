(uiop:define-package :masstimes
  (:use :cl))

(in-package :masstimes)

(defparameter *base-url*
  (quri:uri "https://apiv4.updateparishdata.org/Churchs/")
  "The URL of the masstimes API.")

(defun retrieve-churches-from-lat-lon (latitude longitude)
  "Retrieve the churches around the area near the provided coordinates."
  (let ((url (quri:copy-uri *base-url*)))
    (setf (quri:uri-query-params url) (list (cons "lat" latitude)
                                            (cons "long" longitude)
                                            (cons "pg" 1)))
    (mapcar (lambda (api-entry)
              (make-instance-from-api-entry api-entry (find-class 'church)))
            (jonathan:parse (nth-value 0 (dex:get url))))))

(defclass church-worship-times ()
  ((time-start :initarg :time-start :accessor time-start)
   (time-end :initarg :time-end :accessor time-end)
   (service-typename :initarg :service-typename :accessor service-typename)
   (language :initarg :language :accessor language)
   (is-perpetual :initarg :is-perpentual :accessor is-perpetual)
   (id :initarg :id :accessor id)
   (day-of-week :initarg :day-of-week :accessor day-of-week)
   (comment :initarg :comment :accessor comment)))

(defclass church ()
  ((church-address-city-name :initarg :church-address-city-name
                             :accessor church-address-city-name)
   (church-address-country-territory-name :initarg :church-address-country-territory-name
                                          :accessor church-address-country-territory-name)
   (church-address-county :initarg :church-address-county
                          :accessor church-address-county)
   (church-address-postal-code :initarg :church-address-postal-code
                               :accessor church-address-postal-code)
   (church-address-providence-name :initarg :church-address-providence-name
                                   :accessor :church-address-providence-name)
   (church-address-street-address :initarg :church-address-street-address
                                  :accessor :church-address-street-address)
   (church-type-name :initarg :church-type-name
                     :accessor :church-type-name)
   (church-worship-times :initarg :church-worship-times
                         :accessor :church-worship-times)
   (comments :initarg :comments
             :accessor comments)
   (diocese-name :initarg :diocese-name
                 :accessor :diocese-name)
   (diocese-type-name :initarg :diocese-type-name
                      :accessor :diocese-type-name)
   (directions :initarg :directions
               :accessor :directions)
   (distance :initarg :distance
             :accessor :distance)
   (email :initarg :email
          :accessor :email)
   (id :initarg :id
       :accessor :id)
   (language-name :initarg :language-name
                  :accessor :language-name)
   (last-update :initarg :last-update
                :accessor :last-update)
   (lat-long-source :initarg :lat-long-source
                    :accessor :lat-long-source)
   (latitude :initarg :latitude
             :accessor :latitude)
   (longitude :initarg :longitude
              :accessor :longitude)
   (military-time :initarg :military-time
                  :accessor :military-time)
   (name :initarg :name
         :accessor :name)
   (pastors-name :initarg :pastors-name
                 :accessor :pastors-name)
   (phone-number :initarg :phone-number
                 :accessor :phone-number)
   (resultid :initarg :resultid
             :accessor :resultid)
   (rite-type-name :initarg :rite-type-name
                   :accessor :rite-type-name)
   (url :initarg :url
        :accessor :url)
   (wheel-chair-access :initarg :wheel-chair-access
                       :accessor :wheel-chair-access)))

(defun clean-key (key)
  "Make keyword from API field into kebab-case symbol."
  (intern (string-upcase (cl-slug:slugify (string key)))))

(defun make-instance-from-api-entry (api-entry class)
  "Initialize a church instance from a church entry that comes from the API."
  (let ((instance (make-instance (class-name class))))
    (loop for (key value) on api-entry by #'cddr
          doing (let ((clean-key (clean-key key)))
                  (setf (slot-value instance clean-key)
                        (if (consp value)
                            (mapcar (lambda (sub-value)
                                      (make-instance-from-api-entry sub-value (find-class clean-key)))
                                    value)
                            value))))
    instance))

