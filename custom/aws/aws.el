;;; aws.el --- An interactive tool to query aws
;;
;; Public domain.

;; Author: John Pradeep <yehohanan7@gmail.com>
;; Maintainer: John Pradeep <yehohanan7@gmail.com>
;; Keywords: aws

;;
;; This tool is to do trigger aws commands
;; 
(require 'json)
(require 's)
(require 'names-dev)
(defvar aws-regions '("us-east-1" "us-west-1" "eu-west-1"))

(defmacro with-temp-buffer (&rest body)
  `(with-output-to-temp-buffer "**aws**"
     (with-current-buffer "**aws**"
       ,@body)))

(defun json-to-hash (json)
  (let* ((json-object-type 'hash-table))
    (json-read-from-string json)))

(defun print-output (region output)
  (let ((instances (json-to-hash output)))
    (mapc 'print-instance instances)))

(defun run-command (command region profile)
  "Runs the aws command & returns the output"
  (shell-command-to-string (format "aws %s --profile %s --region %s" command profile region)))

(defun print-instance (instance)
  (when (> (length instance) 0)
    (let* ((instance (aref instance 0))
           (id (gethash "id" instance))
           (status (gethash "status" instance))
           (ip (gethash "ip" instance))
           (type (gethash "type" instance))
           (group (gethash "group" instance)))
      (print (s-join "   " (list id type status ip group))))))


(defun ec2 (command region profile)
  (run-command (format "ec2 %s" command) region profile))

(defun ec2-instances (profile instance-name)
  (interactive "sProfile:\nsName:")
  (let ((command (format "describe-instances --filters \"Name=tag:Name, Values=%s\" --query \"Reservations[*].Instances[*].{id:InstanceId, status:State.Name, type:InstanceType, ip:PrivateIpAddress, group:SecurityGroups[0].GroupName}\"" instance-name)))
    (with-temp-buffer
      (dolist (region aws-regions)
        (print-output region (ec2 command region profile))))))
