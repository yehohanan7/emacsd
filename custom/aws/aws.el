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

(defun group-name (instance)
  (let ((security-groups (gethash "SecurityGroups" instance)))
    (when (> (length security-groups) 0)
      (gethash "GroupName" (aref security-groups 0)))))

(defun print-instance (instance)
  (let ((id (gethash "InstanceId" instance))
        (group (group-name instance))
        (name (gethash "KeyName" instance))
        (state (gethash "Name" (gethash "State" instance)))
        (ip (gethash "PrivateIpAddress" instance))
        (launch-time (gethash "LaunchTime" instance))
        (type (gethash "InstanceType" instance)))
    (print (s-join "   " (list id type name state ip group launch-time)))))

(defun print-reservation (reservation)
  (let ((instances (gethash "Instances" reservation)))
    (mapc 'print-instance instances)))

(defun print-output (region output)
  (let ((reservations (gethash "Reservations" (json-to-hash output))))
    (when (> (length reservations) 0) 
      (print region)
      (mapc 'print-reservation reservations))))

(defun run-command (command region profile)
  "Runs the aws command & returns the output"
  (shell-command-to-string (format "aws %s --profile %s --region %s" command profile region)))

(defun ec2 (command region profile)
  (run-command (format "ec2 %s" command) region profile))

(defun ec2-instances (profile instance-name)
  (interactive "sProfile:\nsName:")
  (let ((command (format "describe-instances --filters \"Name=tag:Name, Values=%s\"" instance-name)))
    (with-temp-buffer
      (dolist (region aws-regions)
        (print-output region (ec2 command region profile))))))


