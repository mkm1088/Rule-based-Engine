;;;======================================================
;;;   ICT Jobs and Courses Recommendation System
;;;
;;;     This expert system recommends some jobs and 
;;;     courses according to uses' background skills, 
;;;     future planning and personality.
;;;
;;;     Author: Liu Jun
;;;     Time: 10/2/2018 09:17
;;;======================================================

;;;***************************
;;;* Configuration           *
;;;***************************

;;; console, cgi, or gui   
(defglobal ?*target* = gui)
(defglobal ?*message* = ResultMessage) 

;;; ***************************
;;; * DEFTEMPLATES & DEFFACTS *
;;; ***************************

(deftemplate MAIN::text-for-id
	(slot id)
	(slot text))

(deftemplate UI-state
	(slot id (default-dynamic (gensym*)))
	(slot state (default middle))
	(slot display)
	(slot relation-asserted (default none))
	(slot response (default none))
	(multislot valid-answers)
	(multislot display-answers)
	(multislot display-results)
	(slot competency (default none)))
   
;;;***************************
;;;* DEFFUNCTION DEFINITIONS *
;;;***************************

;; Search for the text-for-id fact
;; with the same id as ?id
(deffunction MAIN::find-text-for-id (?id)
	(bind ?fact
		(find-fact ((?f text-for-id))
					(eq ?f:id ?id)))
	(if ?fact then
		(fact-slot-value (nth$ 1 ?fact) text)
	else
		?id))

(deffunction MAIN::translate-av (?values)
	;; Create the return value
	(bind ?result (create$))
	;; Iterate over each of the allowed-values
	(progn$ (?v ?values)
		;; Find the associated text-for-id fact
		(bind ?nv
			(find-text-for-id ?v))
		;; Add the text to the return value
		(bind ?result (create$ ?result ?nv)))
	;; Return the return value
	?result)

(deffunction MAIN::replace-spaces (?str)
	(bind ?len (str-length ?str))
	(bind ?i (str-index " " ?str))
	(while (neq ?i FALSE)
		(bind ?str (str-cat (sub-string 1 (- ?i 1) ?str) "-" (sub-string (+ ?i 1) ?len ?str)))
		(bind ?i (str-index " " ?str)))
	?str)

(deffunction MAIN::sym-cat-multifield (?values)
	(bind ?rv (create$))
	(progn$ (?v ?values)
		(bind ?rv (create$ ?rv (sym-cat (replace-spaces ?v)))))
	?rv)

(deffunction MAIN::multifield-to-delimited-string (?mv ?delimiter)
	(bind ?rv "")
	(bind ?first TRUE)
	(progn$ (?v ?mv)
		(if ?first then
			(bind ?first FALSE)
			(bind ?rv (str-cat ?v))
		else
			(bind ?rv (str-cat ?rv ?delimiter ?v))))
	?rv)

(deffunction MAIN::calculate-competency (?working-in-ict
										 ?same-current-job
										 ?working-current-job
										 ?indicate-current-job
										 ?indicate-ideal-job
										 ?it-skills)
	;;; If working-in-ict is Yes, and same-current-job is Yes, cacluating competency
	(if (and (= (str-compare ?working-in-ict "Yes") 0)
			(lexemep ?same-current-job)
			(= (str-compare ?same-current-job "Yes") 0)) then
		;;; Transfering working-current-job
		(switch ?indicate-current-job
			(case AllTNoM	 then (bind ?current-dp 1))
			(case MoreTLessM then (bind ?current-dp 2))
			(case SameTAndM  then (bind ?current-dp 3))
			(case MoreMLessT then (bind ?current-dp 4))
			(case AllMNoT    then (bind ?current-dp 5)))

		;;; Transfering indicate-ideal-job
		(switch ?indicate-ideal-job
			(case AllTNoM	 then (bind ?ideal-dp 1))
			(case MoreTLessM then (bind ?ideal-dp 2))
			(case SameTAndM  then (bind ?ideal-dp 3))
			(case MoreMLessT then (bind ?ideal-dp 4))
			(case AllMNoT	 then (bind ?ideal-dp 5)))

		;;; Technical ideal job
		(if (and (< ?ideal-dp 3) (or (= ?ideal-dp 3) (< ?current-dp ?ideal-dp))) then
			;;; Transfering working-current-job
			(switch ?working-current-job
				(case WorkingYear0to3    then (bind ?work-competency -3) (bind ?it-competency -3))
				(case WorkingYear3to5    then (bind ?work-competency -2) (bind ?it-competency -2))
				(case WorkingYear5to10   then (bind ?work-competency -1) (bind ?it-competency -1))
				(case WorkingYearAbove10 then (bind ?work-competency 0)  (bind ?it-competency 0)))
		else
			;;; Transfering working-current-job
			(switch ?working-current-job
				(case WorkingYear0to3    then (bind ?work-competency 3) (bind ?it-competency 3))
				(case WorkingYear3to5    then (bind ?work-competency 2) (bind ?it-competency 2))
				(case WorkingYear5to10   then (bind ?work-competency 1) (bind ?it-competency 1))
				(case WorkingYearAbove10 then (bind ?work-competency 0) (bind ?it-competency 0))))

		;;; competency = abs(0.25 * (ideal-dp - current-dp) + 0.5 * work-competency + 0.25 * it-competency)
		(bind ?competency (abs (+ (+ (* 0.25 (- ?ideal-dp ?current-dp)) (* 0.5 ?work-competency)) (* 0.25 ?it-competency))))

		(if (>= ?competency 2) then 
			(bind ?competency 3))
		(if (and (< ?competency 2) (>= ?competency 1)) then
			(bind ?competency 4))
		(if (and (< ?competency 1) (>= ?competency 0)) then
			(bind ?competency 5))
	else
		(bind ?competency 3))
	;;; return competency
	?competency)

(deffunction MAIN::match-personality (?mind-types 
									  ?see-things 
									  ?judge-things 
									  ?act-towards-changes
									  ?values)
	(bind ?largest-count 0)
	(bind ?largest-v "")
	;; Iterate over each of the allowed-values
	(progn$ (?v ?values)
		;; Get each detail personality of v
		(bind ?mind-types-v (sub-string 1 1 ?v))
		(bind ?see-things-v (sub-string 2 2 ?v))
		(bind ?judge-things-v (sub-string 3 3 ?v))
		(bind ?act-towards-changes-v (sub-string 4 4 ?v))
		(bind ?count 0)
		(if (= (str-compare ?mind-types ?mind-types-v) 0) then 
			(bind ?count (+ ?count 1)))
		(if (= (str-compare ?see-things ?see-things-v) 0) then 
			(bind ?count (+ ?count 1)))
		(if (= (str-compare ?judge-things ?judge-things-v) 0) then 
			(bind ?count (+ ?count 1)))
		(if (= (str-compare ?act-towards-changes ?act-towards-changes-v) 0) then 
			(bind ?count (+ ?count 1)))
		(if (>= ?count ?largest-count) then 
			(bind ?largest-count ?count)
			(bind ?largest-v ?v)))
	?largest-v)

;;;***************************
;;;* STATE METHODS           *
;;;***************************

;;; GUI target (iOS and JNI)

(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
	(assert (UI-state (state ?state)
					  (display ?message)
                      (relation-asserted greeting)
                      (valid-answers Yes)
                      (display-answers Yes)
					  (display-results)))
    (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
    (assert (UI-state (state ?state)
					  (display ?message)
                      (relation-asserted ?relation-asserted)
                      (response ?response)
                      (valid-answers ?valid-answers)
                      (display-answers ?display-answers)
					  (display-results)))
    (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD)
						 (?competency INTEGER))
    (assert (UI-state (state ?state)
					  (display ?message)
                      (relation-asserted ?relation-asserted)
                      (response ?response)
                      (valid-answers ?valid-answers)
                      (display-answers ?display-answers)
					  (display-results)
					  (competency ?competency)))
    (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?target SYMBOL (eq ?target gui))
                         (?display LEXEME)
						 (?display-results MULTIFIELD))
   (assert (UI-state (state ?state)
					 (display ?display)
                     (valid-answers)
                     (display-answers)
					 (display-results ?display-results)))
   (assert (conclusion))
   (halt))

;;;***************************
;;;* STARTUP RULE            *
;;;***************************

(defrule system-banner ""
	(not (greeting Yes))
	=>
	(handle-state greeting
				  ?*target*
                  (find-text-for-id WelcomeMessage)
                  greeting
                  (create$)))
  
;;;***************************
;;;* QUERY RULES             *
;;;***************************

;;; Have you been working in ICT domain?
(defrule determine-working-in-ict ""
	(greeting Yes)
    (not (working-in-ict ?))
    =>
    (bind ?answers (create$ Yes No))
    (handle-state interview
                  ?*target*
                  (find-text-for-id WorkingInICTQuestion)
                  working-in-ict
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)))

;;; Which following job groups are you interested in?
(defrule determine-job-groups ""
	(working-in-ict ?)
    (not (job-groups ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ JobGroupsBA
							JobGroupsCC
							JobGroupsDDM
							JobGroupsINS
							JobGroupsISM
	  					    JobGroupsITM
						    JobGroupsSDD
						    ))
	(handle-state interview
                  ?*target*
                  (find-text-for-id JobGroupsQuestion)
                  job-groups
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)))

;;; Is that same/similar with your current job?
(defrule determine-same-current-job ""
	(working-in-ict Yes)
	(job-groups ?)
	(not (same-current-job ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ Yes No))
    (handle-state interview
                  ?*target*
                  (find-text-for-id SameCurrentJobQuestion)
                  same-current-job
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)))

;;; How long have you been working in your current job?
(defrule determine-working-current-job-ict ""
	(working-in-ict Yes)
	(same-current-job ?)
    (not (working-current-job ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ WorkingYear0to3
						    WorkingYear3to5
						    WorkingYear5to10
						    WorkingYearAbove10))
	(handle-state interview
                  ?*target*
                  (find-text-for-id WorkingCurrentJobQuestion)
                  working-current-job
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)))

(defrule determine-working-current-job-notict ""
	(working-in-ict No)
	(job-groups ?)
    (not (working-current-job ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ WorkingYear0to3
						    WorkingYear3to5
						    WorkingYear5to10
						    WorkingYearAbove10))
	(handle-state interview
                  ?*target*
                  (find-text-for-id WorkingCurrentJobQuestion)
                  working-current-job
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)))

;;; Can you indicate your current job nature?
(defrule determine-indicate-current-job ""
	(working-current-job ?)
	(not (indicate-current-job ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ AllTNoM
						    MoreTLessM
						    SameTAndM
						    MoreMLessT
						    AllMNoT))
	(handle-state interview
                  ?*target*
                  (find-text-for-id IndicateCurrentJobQuestion)
                  indicate-current-job
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)))

;;; Can you indicate your ideal job nature?
(defrule determine-indicate-ideal-job ""
	(indicate-current-job ?)
	(not (indicate-ideal-job ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ AllTNoM
						    MoreTLessM
						    SameTAndM
						    MoreMLessT
						    AllMNoT))
    (handle-state interview
                  ?*target*
                  (find-text-for-id IndicateIdealJobQuestion)
                  indicate-ideal-job
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)))

;;; What is your current IT related skills?
(defrule determine-it-skills ""
	(indicate-ideal-job ?)
	(not (it-skills ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ VeryLow
						    Low
						    Medium
						    High
						    VeryHigh))
    (handle-state interview
                  ?*target*
                  (find-text-for-id ITSkillsQuestion)
                  it-skills
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)))

;;; What do you value most?
(defrule determine-value-same-job ""
	(working-in-ict ?working-in-ict)
	(same-current-job ?same-current-job)
	(working-current-job ?working-current-job)
	(indicate-current-job ?indicate-current-job)
	(indicate-ideal-job ?indicate-ideal-job)
	(it-skills ?it-skills)
	(not (value-most ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ ValueCP
						    ValueWWO
						    ValueD))
	;;; Calculate competency
	(bind ?competency (calculate-competency ?working-in-ict
											?same-current-job
											?working-current-job
											?indicate-current-job
											?indicate-ideal-job
											?it-skills))
    (handle-state interview
                  ?*target*
                  (find-text-for-id ValueMostQuestion)
                  value-most
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)
				  ?competency))

(defrule determine-value-notsame-job ""
	(working-in-ict ?working-in-ict)
	(working-current-job ?working-current-job)
	(indicate-current-job ?indicate-current-job)
	(indicate-ideal-job ?indicate-ideal-job)
	(it-skills ?it-skills)
	(not (value-most ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ ValueCP
						    ValueWWO
						    ValueD))
	;;; Calculate competency
	(bind ?competency (calculate-competency ?working-in-ict
											"No"
											?working-current-job
											?indicate-current-job
											?indicate-ideal-job
											?it-skills))
    (handle-state interview
                  ?*target*
                  (find-text-for-id ValueMostQuestion)
                  value-most
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)
				  ?competency))

;;; What is your mind types?
(defrule determine-mind-types ""
	(value-most ?)
	(not (mind-types ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ PersonalityI
						    PersonalityE))
    (handle-state interview
                  ?*target*
                  (find-text-for-id MindTypesQuestion)
                  mind-types
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)))

;;; How do you see things?
(defrule determine-see-things ""
	(mind-types ?)
	(not (see-things ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ PersonalityS
						    PersonalityN))
    (handle-state interview
                  ?*target*
                  (find-text-for-id SeeThingsQuestion)
                  see-things
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)))

;;; How do you judge things?
(defrule determine-judge-things ""
	(see-things ?)
    (not (judge-things ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ PersonalityT
						    PersonalityF))
    (handle-state interview
                  ?*target*
                  (find-text-for-id JudgeThingsQuestion)
                  judge-things
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)))

;;; How do you usually act towards changes?
(defrule determine-act-towards-changes ""
	(judge-things ?)
    (not (act-towards-changes ?))
    (not (conclusion))
    =>
    (bind ?answers (create$ PersonalityJ
						    PersonalityP))
    (handle-state interview
                  ?*target*
                  (find-text-for-id ActTowardsChangesQuestion)
                  act-towards-changes
                  (nth$ 1 ?answers)
                  ?answers
                  (translate-av ?answers)))

;;;***************************
;;;* RECOMMENDATION RULES    *
;;;***************************
(defrule ba-cp-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsBA)
	(value-most ValueCP)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ISTJ" "ISFJ")))
    (if (= (str-compare ?personality-reslut "ISTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job032) ";" 
								   (find-text-for-id Course032) ";" 
								   (find-text-for-id Course077) ";"
								   (find-text-for-id Provider013) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job033) ";" 
								   (find-text-for-id Course032) ";" 
								   (find-text-for-id Course011) ";"
								   (find-text-for-id Provider013) ";"
								   (find-text-for-id Provider022))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule ba-cp-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsBA)
	(value-most ValueCP)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENFJ" "ENTJ" "ESTJ" "ISFJ" 
														  "ISTJ" "ISTP")))
    (if (= (str-compare ?personality-reslut "ENFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job039) ";" 
								   (find-text-for-id Course008) ";" 
								   (find-text-for-id Course032) ";"
								   (find-text-for-id Provider015) ";"
								   (find-text-for-id Provider013))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ENTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job086) ";" 
								   (find-text-for-id Course062) ";" 
								   (find-text-for-id Course079) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider029))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ESTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job013) ";" 
								   (find-text-for-id Course094) ";" 
								   (find-text-for-id Course008) ";"
								   (find-text-for-id Provider025) ";"
								   (find-text-for-id Provider014))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ISFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job089) ";" 
								   (find-text-for-id Course069) ";" 
								   (find-text-for-id Course093) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider025))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ISTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job070) ";" 
								   (find-text-for-id Course055) ";" 
								   (find-text-for-id Course069) ";"
								   (find-text-for-id Provider020) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ISTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job100) ";" 
								   (find-text-for-id Course082) ";" 
								   (find-text-for-id Course093) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider025))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule ba-cp-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsBA)
	(value-most ValueCP)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
	
(defrule ba-wwo-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsBA)
	(value-most ValueWWO)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))

(defrule ba-wwo-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsBA)
	(value-most ValueWWO)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ISFP")))
	(if (= (str-compare ?personality-reslut "ISFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job060) ";" 
								   (find-text-for-id Course006) ";" 
								   (find-text-for-id Course087) ";"
								   (find-text-for-id Provider009) ";"
								   (find-text-for-id Provider027))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
	
(defrule ba-wwo-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsBA)
	(value-most ValueWWO)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
	
(defrule ba-d-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsBA)
	(value-most ValueD)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 5)))
	
(defrule ba-d-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsBA)
	(value-most ValueD)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 5)))

(defrule ba-d-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsBA)
	(value-most ValueD)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENTJ" "INTP")))
    (if (= (str-compare ?personality-reslut "ENTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job019) ";" 
								   (find-text-for-id Course065) ";" 
								   (find-text-for-id Course081) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "INTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job045) ";" 
								   (find-text-for-id Course003) ";" 
								   (find-text-for-id Course090) ";"
								   (find-text-for-id Provider028) ";"
								   (find-text-for-id Provider001))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule cc-cp-extrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsCC)
	(value-most ValueCP)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ISTJ")))
    (if (= (str-compare ?personality-reslut "ISTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job025) ";" 
								   (find-text-for-id Course002) ";" 
								   (find-text-for-id Course022) ";"
								   (find-text-for-id Provider008) ";"
								   (find-text-for-id Provider031))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule cc-cp-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsCC)
	(value-most ValueCP)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ESFJ" "ESFP" "ESTJ" "INTJ"
														  "ISTJ" "ISTP")))
    (if (= (str-compare ?personality-reslut "ESFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job047) ";" 
								   (find-text-for-id Course063) ";" 
								   (find-text-for-id Course047) ";"
								   (find-text-for-id Provider020) ";"
								   (find-text-for-id Provider006))
								(str-cat (find-text-for-id Job052) ";" 
								   (find-text-for-id Course029) ";" 
								   (find-text-for-id Course030) ";"
								   (find-text-for-id Provider013) ";"
								   (find-text-for-id Provider013))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ESFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job027) ";" 
								   (find-text-for-id Course010) ";" 
								   (find-text-for-id Course066) ";"
								   (find-text-for-id Provider014) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))	
    (if (= (str-compare ?personality-reslut "ESTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job024) ";" 
								   (find-text-for-id Course031) ";" 
								   (find-text-for-id Course030) ";"
								   (find-text-for-id Provider013) ";"
								   (find-text-for-id Provider013))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))		
    (if (= (str-compare ?personality-reslut "INTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job054) ";" 
								   (find-text-for-id Course030) ";" 
								   (find-text-for-id Course096) ";"
								   (find-text-for-id Provider013) ";"
								   (find-text-for-id Provider025))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job029) ";" 
								   (find-text-for-id Course047) ";" 
								   (find-text-for-id Course046) ";"
								   (find-text-for-id Provider006) ";"
								   (find-text-for-id Provider006))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job085) ";" 
								   (find-text-for-id Course010) ";" 
								   (find-text-for-id Course030) ";"
								   (find-text-for-id Provider014) ";"
								   (find-text-for-id Provider013))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule cc-cp-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsCC)
	(value-most ValueCP)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENFJ")))
    (if (= (str-compare ?personality-reslut "ENFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job018) ";" 
								   (find-text-for-id Course029) ";" 
								   (find-text-for-id Course010) ";"
								   (find-text-for-id Provider013) ";"
								   (find-text-for-id Provider014))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule cc-wwo-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsCC)
	(value-most ValueWWO)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
		
(defrule cc-wwo-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsCC)
	(value-most ValueWWO)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENFP" "ESTP" "INFP" "ISFP")))

    (if (= (str-compare ?personality-reslut "ENFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job028) ";" 
								   (find-text-for-id Course049) ";" 
								   (find-text-for-id Course010) ";"
								   (find-text-for-id Provider010) ";"
								   (find-text-for-id Provider014))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))	
    (if (= (str-compare ?personality-reslut "ESTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job026) ";" 
								   (find-text-for-id Course010) ";" 
								   (find-text-for-id Course005) ";"
								   (find-text-for-id Provider014) ";"
								   (find-text-for-id Provider009))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "INFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job023) ";" 
								   (find-text-for-id Course009) ";" 
								   (find-text-for-id Course006) ";"
								   (find-text-for-id Provider014) ";"
								   (find-text-for-id Provider009))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job005) ";" 
								   (find-text-for-id Course009) ";" 
								   (find-text-for-id Course031) ";"
								   (find-text-for-id Provider014) ";"
								   (find-text-for-id Provider013))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule cc-wwo-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsCC)
	(value-most ValueWWO)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))

(defrule cc-d-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsCC)
	(value-most ValueD)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (value-most ValueCP)))
	
(defrule cc-d-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsCC)
	(value-most ValueD)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (value-most ValueCP)))
	
(defrule cc-d-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsCC)
	(value-most ValueD)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (value-most ValueCP)))

(defrule ddm-cp-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsDDM)
	(value-most ValueCP)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ISFJ" "ESTJ")))
    (if (= (str-compare ?personality-reslut "ISFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job007) ";" 
								   (find-text-for-id Course046) ";" 
								   (find-text-for-id Course048) ";"
								   (find-text-for-id Provider006) ";"
								   (find-text-for-id Provider010))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ESTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job038) ";" 
								   (find-text-for-id Course024) ";" 
								   (find-text-for-id Course039) ";"
								   (find-text-for-id Provider012) ";"
								   (find-text-for-id Provider007))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule ddm-cp-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsDDM)
	(value-most ValueCP)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENFJ" "INTJ" "ISFJ")))
    (if (= (str-compare ?personality-reslut "ENFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job041) ";" 
								   (find-text-for-id Course046) ";" 
								   (find-text-for-id Course076) ";"
								   (find-text-for-id Provider006) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "INTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job034) ";" 
								   (find-text-for-id Course024) ";" 
								   (find-text-for-id Course076) ";"
								   (find-text-for-id Provider012) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job040) ";" 
								   (find-text-for-id Course084) ";" 
								   (find-text-for-id Course097) ";"
								   (find-text-for-id Provider009) ";"
								   (find-text-for-id Provider025))
								(str-cat (find-text-for-id Job036) ";" 
								   (find-text-for-id Course025) ";" 
								   (find-text-for-id Course024) ";"
								   (find-text-for-id Provider012) ";"
								   (find-text-for-id Provider012))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule ddm-cp-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsDDM)
	(value-most ValueCP)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
	
(defrule ddm-wwo-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsDDM)
	(value-most ValueWWO)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))

(defrule ddm-wwo-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsDDM)
	(value-most ValueWWO)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "INTP")))
    (if (= (str-compare ?personality-reslut "INTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job037) ";" 
								   (find-text-for-id Course024) ";" 
								   (find-text-for-id Course090) ";"
								   (find-text-for-id Provider012) ";"
								   (find-text-for-id Provider001))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule ddm-wwo-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsDDM)
	(value-most ValueWWO)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
	
(defrule ddm-d-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsDDM)
	(value-most ValueD)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 5)))

(defrule ddm-d-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsDDM)
	(value-most ValueD)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 5)))

(defrule ddm-d-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsDDM)
	(value-most ValueD)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "INTP")))
    (if (= (str-compare ?personality-reslut "INTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job035) ";" 
								   (find-text-for-id Course076) ";" 
								   (find-text-for-id Course099) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider009))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
	
(defrule ins-cp-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsINS)
	(value-most ValueCP)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ESFJ" "ISFJ" "ISTJ")))
    (if (= (str-compare ?personality-reslut "ESFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job010) ";" 
								   (find-text-for-id Course027) ";" 
								   (find-text-for-id Course085) ";"
								   (find-text-for-id Provider003) ";"
								   (find-text-for-id Provider030))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job009) ";" 
								   (find-text-for-id Course021) ";" 
								   (find-text-for-id Course028) ";"
								   (find-text-for-id Provider005) ";"
								   (find-text-for-id Provider003))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job011) ";" 
								   (find-text-for-id Course026) ";" 
								   (find-text-for-id Course039) ";"
								   (find-text-for-id Provider003) ";"
								   (find-text-for-id Provider007))
								(str-cat (find-text-for-id Job066) ";" 
								   (find-text-for-id Course021) ";" 
								   (find-text-for-id Course001) ";"
								   (find-text-for-id Provider005) ";"
								   (find-text-for-id Provider002))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))		
		
(defrule ins-cp-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsINS)
	(value-most ValueCP)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENFJ" "INFJ" "INTJ" "ISFJ" 
														  "ISTJ" "ISTP")))
    (if (= (str-compare ?personality-reslut "ENFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job065) ";" 
								   (find-text-for-id Course021) ";" 
								   (find-text-for-id Course076) ";"
								   (find-text-for-id Provider005) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "INFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job069) ";" 
								   (find-text-for-id Course028) ";" 
								   (find-text-for-id Course020) ";"
								   (find-text-for-id Provider003) ";"
								   (find-text-for-id Provider016))
								(str-cat (find-text-for-id Job093) ";" 
								   (find-text-for-id Course058) ";" 
								   (find-text-for-id Course045) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider006))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "INTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job083) ";" 
								   (find-text-for-id Course085) ";" 
								   (find-text-for-id Course028) ";"
								   (find-text-for-id Provider030) ";"
								   (find-text-for-id Provider003))
								(str-cat (find-text-for-id Job022) ";" 
								   (find-text-for-id Course016) ";" 
								   (find-text-for-id Course051) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job042) ";" 
								   (find-text-for-id Course083) ";" 
								   (find-text-for-id Course084) ";"
								   (find-text-for-id Provider009) ";"
								   (find-text-for-id Provider009))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job084) ";" 
								   (find-text-for-id Course085) ";" 
								   (find-text-for-id Course023) ";"
								   (find-text-for-id Provider030) ";"
								   (find-text-for-id Provider011))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job088) ";" 
								   (find-text-for-id Course085) ";" 
								   (find-text-for-id Course028) ";"
								   (find-text-for-id Provider030) ";"
								   (find-text-for-id Provider003))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule ins-cp-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsINS)
	(value-most ValueCP)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
	
(defrule ins-wwo-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsINS)
	(value-most ValueWWO)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ESFP")))
    (if (= (str-compare ?personality-reslut "ESFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job008) ";" 
								   (find-text-for-id Course015) ";" 
								   (find-text-for-id Course016) ";"
								   (find-text-for-id Provider001) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))		
		
(defrule ins-wwo-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsINS)
	(value-most ValueWWO)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENFP" "ENTJ" "ENTP" "ESTJ" 
														  "INTP" "ISFP")))
    (if (= (str-compare ?personality-reslut "ENFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job055) ";" 
								   (find-text-for-id Course033) ";" 
								   (find-text-for-id Course006) ";"
								   (find-text-for-id Provider012) ";"
								   (find-text-for-id Provider009))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))		
    (if (= (str-compare ?personality-reslut "ENTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job067) ";" 
								   (find-text-for-id Course021) ";" 
								   (find-text-for-id Course090) ";"
								   (find-text-for-id Provider005) ";"
								   (find-text-for-id Provider001))
								(str-cat (find-text-for-id Job068) ";" 
								   (find-text-for-id Course021) ";" 
								   (find-text-for-id Course086) ";"
								   (find-text-for-id Provider005) ";"
								   (find-text-for-id Provider001))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ENTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job087) ";" 
								   (find-text-for-id Course058) ";" 
								   (find-text-for-id Course016) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ESTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job059) ";" 
								   (find-text-for-id Course092) ";" 
								   (find-text-for-id Course073) ";"
								   (find-text-for-id Provider009) ";"
								   (find-text-for-id Provider029))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "INTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job049) ";" 
								   (find-text-for-id Course014) ";" 
								   (find-text-for-id Course016) ";"
								   (find-text-for-id Provider001) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))		
    (if (= (str-compare ?personality-reslut "ISFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job048) ";" 
								   (find-text-for-id Course016) ";" 
								   (find-text-for-id Course058) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule ins-wwo-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsINS)
	(value-most ValueWWO)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))	
	
(defrule ins-d-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsINS)
	(value-most ValueD)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "INFJ")))
    (if (= (str-compare ?personality-reslut "INFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job050) ";" 
								   (find-text-for-id Course016) ";" 
								   (find-text-for-id Course014) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider001))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))		
		
(defrule ins-d-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsINS)
	(value-most ValueD)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 3)))

(defrule ins-d-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsINS)
	(value-most ValueD)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENTP")))
    (if (= (str-compare ?personality-reslut "ENTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job021) ";" 
								   (find-text-for-id Course058) ";" 
								   (find-text-for-id Course071) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule ism-cp-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsISM)
	(value-most ValueCP)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (value-most ValueWWO)))

(defrule ism-cp-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsISM)
	(value-most ValueCP)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (value-most ValueWWO)))

(defrule ism-cp-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsISM)
	(value-most ValueCP)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (value-most ValueWWO)))
	
(defrule ism-wwo-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsISM)
	(value-most ValueWWO)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ESTP" "ESJP" "ESFJ")))
    (if (= (str-compare ?personality-reslut "ESTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job062) ";" 
								   (find-text-for-id Course060) ";" 
								   (find-text-for-id Course059) ";"
								   (find-text-for-id Provider021) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ESJP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job064) ";" 
								   (find-text-for-id Course061) ";" 
								   (find-text-for-id Course062) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ESFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job081) ";" 
								   (find-text-for-id Course059) ";" 
								   (find-text-for-id Course053) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider020))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule ism-wwo-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsISM)
	(value-most ValueWWO)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENFJ" "ENFP" "ENTP" "ESFJ" 
														  "ESFP")))
    (if (= (str-compare ?personality-reslut "ENFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job016) ";" 
								   (find-text-for-id Course040) ";" 
								   (find-text-for-id Course079) ";"
								   (find-text-for-id Provider007) ";"
								   (find-text-for-id Provider029))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ENFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job082) ";" 
								   (find-text-for-id Course040) ";" 
								   (find-text-for-id Course074) ";"
								   (find-text-for-id Provider007) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ENTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job063) ";" 
								   (find-text-for-id Course062) ";" 
								   (find-text-for-id Course056) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ESFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job072) ";" 
								   (find-text-for-id Course068) ";" 
								   (find-text-for-id Course054) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider020))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ESFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job071) ";" 
								   (find-text-for-id Course053) ";" 
								   (find-text-for-id Course054) ";"
								   (find-text-for-id Provider020) ";"
								   (find-text-for-id Provider020))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule ism-wwo-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsISM)
	(value-most ValueWWO)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
	
(defrule ism-d-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsISM)
	(value-most ValueD)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))

(defrule ism-d-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsISM)
	(value-most ValueD)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENFJ" "ENFP" "ESTJ")))
    (if (= (str-compare ?personality-reslut "ENFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job014) ";" 
								   (find-text-for-id Course080) ";" 
								   (find-text-for-id Course050) ";"
								   (find-text-for-id Provider029) ";"
								   (find-text-for-id Provider020))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ENFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job061) ";" 
								   (find-text-for-id Course062) ";" 
								   (find-text-for-id Course040) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider007))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ESTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job080) ";" 
								   (find-text-for-id Course086) ";" 
								   (find-text-for-id Course090) ";"
								   (find-text-for-id Provider024) ";"
								   (find-text-for-id Provider001))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule ism-d-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsISM)
	(value-most ValueD)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ESTP")))
    (if (= (str-compare ?personality-reslut "ESTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job044) ";" 
								   (find-text-for-id Course067) ";" 
								   (find-text-for-id Course080) ";"
								   (find-text-for-id Provider020) ";"
								   (find-text-for-id Provider029))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule itm-cp-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsITM)
	(value-most ValueCP)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ESFP" "INFJ")))
    (if (= (str-compare ?personality-reslut "ESFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job012) ";" 
								   (find-text-for-id Course035) ";" 
								   (find-text-for-id Course097) ";"
								   (find-text-for-id Provider017) ";"
								   (find-text-for-id Provider025))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "INFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job095) ";" 
								   (find-text-for-id Course042) ";" 
								   (find-text-for-id Course100) ";"
								   (find-text-for-id Provider007) ";"
								   (find-text-for-id Provider006))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule itm-cp-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsITM)
	(value-most ValueCP)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENTP" "ESFJ" "INFJ" "INTJ")))
	(if (= (str-compare ?personality-reslut "ENTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job006) ";" 
								   (find-text-for-id Course038) ";" 
								   (find-text-for-id Course089) ";"
								   (find-text-for-id Provider007) ";"
								   (find-text-for-id Provider001))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ESFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job043) ";" 
								   (find-text-for-id Course057) ";" 
								   (find-text-for-id Course065) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider018))
								(str-cat (find-text-for-id Job051) ";" 
								   (find-text-for-id Course076) ";" 
								   (find-text-for-id Course065) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "INFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job091) ";" 
								   (find-text-for-id Course097) ";" 
								   (find-text-for-id Course045) ";"
								   (find-text-for-id Provider025) ";"
								   (find-text-for-id Provider006))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))		
    (if (= (str-compare ?personality-reslut "INTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job096) ";" 
								   (find-text-for-id Course091) ";" 
								   (find-text-for-id Course098) ";"
								   (find-text-for-id Provider009) ";"
								   (find-text-for-id Provider026))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule itm-cp-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsITM)
	(value-most ValueCP)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
	
(defrule itm-wwo-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsITM)
	(value-most ValueWWO)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
		
(defrule itm-wwo-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsITM)
	(value-most ValueWWO)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENFJ" "ENTJ" "ENTP" "ESTP" 
														  "INFP" "INTP")))
    (if (= (str-compare ?personality-reslut "ENFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job077) ";" 
								   (find-text-for-id Course006) ";" 
								   (find-text-for-id Course087) ";"
								   (find-text-for-id Provider009) ";"
								   (find-text-for-id Provider027))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ENTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job094) ";" 
								   (find-text-for-id Course064) ";" 
								   (find-text-for-id Course039) ";"
								   (find-text-for-id Provider020) ";"
								   (find-text-for-id Provider007))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ENTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job076) ";" 
								   (find-text-for-id Course070) ";" 
								   (find-text-for-id Course006) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider009))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ESTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job058) ";" 
								   (find-text-for-id Course007) ";" 
								   (find-text-for-id Course072) ";"
								   (find-text-for-id Provider009) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))	
	(if (= (str-compare ?personality-reslut "INFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job074) ";" 
								   (find-text-for-id Course088) ";" 
								   (find-text-for-id Course080) ";"
								   (find-text-for-id Provider027) ";"
								   (find-text-for-id Provider018))
								(str-cat (find-text-for-id Job056) ";" 
								   (find-text-for-id Course072) ";" 
								   (find-text-for-id Course078) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "INTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job057) ";" 
								   (find-text-for-id Course015) ";" 
								   (find-text-for-id Course038) ";"
								   (find-text-for-id Provider001) ";"
								   (find-text-for-id Provider007))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule itm-wwo-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsITM)
	(value-most ValueWWO)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
	
(defrule itm-d-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsITM)
	(value-most ValueD)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
		
(defrule itm-d-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsITM)
	(value-most ValueD)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ESTP")))
	(if (= (str-compare ?personality-reslut "ESTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job073) ";" 
								   (find-text-for-id Course088) ";" 
								   (find-text-for-id Course090) ";"
								   (find-text-for-id Provider027) ";"
								   (find-text-for-id Provider001))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule itm-d-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsITM)
	(value-most ValueD)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENTP" "ESTP")))
    (if (= (str-compare ?personality-reslut "ENTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job020) ";" 
								   (find-text-for-id Course006) ";" 
								   (find-text-for-id Course065) ";"
								   (find-text-for-id Provider009) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
	(if (= (str-compare ?personality-reslut "ESTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job075) ";" 
								   (find-text-for-id Course006) ";" 
								   (find-text-for-id Course072) ";"
								   (find-text-for-id Provider009) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))

(defrule ssd-cp-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsSDD)
	(value-most ValueCP)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ISTJ")))
    (if (= (str-compare ?personality-reslut "ISTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job002) ";" 
								   (find-text-for-id Course052) ";" 
								   (find-text-for-id Course095) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider025))
								(str-cat (find-text-for-id Job097) ";" 
								   (find-text-for-id Course019) ";" 
								   (find-text-for-id Course075) ";"
								   (find-text-for-id Provider021) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule ssd-cp-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsSDD)
	(value-most ValueCP)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ESFJ" "ESTJ" "INTJ" "ISFJ"
														  "ISTJ" "ISTP")))
    (if (= (str-compare ?personality-reslut "ESFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job046) ";" 
								   (find-text-for-id Course044) ";" 
								   (find-text-for-id Course063) ";"
								   (find-text-for-id Provider006) ";"
								   (find-text-for-id Provider020))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))	
    (if (= (str-compare ?personality-reslut "ESTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job078) ";" 
								   (find-text-for-id Course034) ";" 
								   (find-text-for-id Course018) ";"
								   (find-text-for-id Provider007) ";"
								   (find-text-for-id Provider023))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))		
    (if (= (str-compare ?personality-reslut "INTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job099) ";" 
								   (find-text-for-id Course036) ";" 
								   (find-text-for-id Course013) ";"
								   (find-text-for-id Provider019) ";"
								   (find-text-for-id Provider018))
								(str-cat (find-text-for-id Job053) ";" 
								   (find-text-for-id Course004) ";" 
								   (find-text-for-id Course076) ";"
								   (find-text-for-id Provider004) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))			
    (if (= (str-compare ?personality-reslut "ISFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job030) ";" 
								   (find-text-for-id Course034) ";" 
								   (find-text-for-id Course015) ";"
								   (find-text-for-id Provider007) ";"
								   (find-text-for-id Provider001))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISTJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job090) ";" 
								   (find-text-for-id Course068) ";" 
								   (find-text-for-id Course043) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider009))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job092) ";" 
								   (find-text-for-id Course054) ";" 
								   (find-text-for-id Course043) ";"
								   (find-text-for-id Provider020) ";"
								   (find-text-for-id Provider009))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule ssd-cp-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsSDD)
	(value-most ValueCP)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ENFJ")))
    (if (= (str-compare ?personality-reslut "ENFJ") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job017) ";" 
								   (find-text-for-id Course065) ";" 
								   (find-text-for-id Course081) ";"
								   (find-text-for-id Provider018) ";"
								   (find-text-for-id Provider018))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule ssd-wwo-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsSDD)
	(value-most ValueWWO)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
		
(defrule ssd-wwo-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsSDD)
	(value-most ValueWWO)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?personality-reslut (match-personality ?mind-types 
											     ?see-things 
											     ?judge-things 
											     ?act-towards-changes
												 (create$ "ESFP" "INFP" "INTP" "ISFP")))
    (if (= (str-compare ?personality-reslut "ESFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job015) ";" 
								   (find-text-for-id Course005) ";" 
								   (find-text-for-id Course041) ";"
								   (find-text-for-id Provider009) ";"
								   (find-text-for-id Provider007))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "INFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job001) ";" 
								   (find-text-for-id Course087) ";" 
								   (find-text-for-id Course072) ";"
								   (find-text-for-id Provider027) ";"
								   (find-text-for-id Provider018))
								(str-cat (find-text-for-id Job031) ";" 
								   (find-text-for-id Course038) ";" 
								   (find-text-for-id Course042) ";"
								   (find-text-for-id Provider007) ";"
								   (find-text-for-id Provider007))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))					
    (if (= (str-compare ?personality-reslut "INTP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job079) ";" 
								   (find-text-for-id Course018) ";" 
								   (find-text-for-id Course086) ";"
								   (find-text-for-id Provider023) ";"
								   (find-text-for-id Provider024))
								(str-cat (find-text-for-id Job098) ";" 
								   (find-text-for-id Course037) ";" 
								   (find-text-for-id Course017) ";"
								   (find-text-for-id Provider007) ";"
								   (find-text-for-id Provider021))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results))
    (if (= (str-compare ?personality-reslut "ISFP") 0) then
		(bind ?results (create$ (str-cat (find-text-for-id Job003) ";" 
								   (find-text-for-id Course012) ";" 
								   (find-text-for-id Course038) ";"
								   (find-text-for-id Provider022) ";"
								   (find-text-for-id Provider007))
								(str-cat (find-text-for-id Job004) ";" 
								   (find-text-for-id Course012) ";" 
								   (find-text-for-id Course038) ";"
								   (find-text-for-id Provider022) ";"
								   (find-text-for-id Provider007))))
		(handle-state conclusion ?*target* (find-text-for-id ?*message*) ?results)))
		
(defrule ssd-wwo-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsSDD)
	(value-most ValueWWO)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (competency 4)))
	
(defrule ssd-d-entrant-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsSDD)
	(value-most ValueD)
    (competency 3)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (value-most ValueCP)))
		
(defrule ssd-d-expert-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsSDD)
	(value-most ValueD)
    (competency 4)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (value-most ValueCP)))
		
(defrule ssd-d-senior-conclusions ""
	(declare (salience 10))
    (job-groups JobGroupsSDD)
	(value-most ValueD)
    (competency 5)
    (mind-types ?mind-types)
    (see-things ?see-things)
    (judge-things ?judge-things)
    (act-towards-changes ?act-towards-changes)
    =>
	(bind ?*message* SecResultMessage)
    (assert (value-most ValueCP)))