<?php

class YiidisController extends CController {

    public $user;

    public function beforeAction() {
      try {
        $this->user = Yii::app()->facebook->getUser();
      } catch (FacebookNeedsLogin $e) {
        Yii::app()->facebook->doLogin();
        return false;
      }

      return true;
    }

    public function getSession() {
      $sessionID = Yii::app()->session->sessionID;
      return YiidisSession::ensure($sessionID);
    }

}