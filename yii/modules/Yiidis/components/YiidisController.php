<?php

class YiidisController extends CController {

  public $requireLogin = true;
  public $user;
  
  public function beforeAction() {
    return $this->getUser();
  }

  public function getUser($login = null) {
    if ($login === null) $login = $this->requireLogin;
    try {
      $this->user = Yii::app()->facebook->getUser($login);
    } catch (FacebookNeedsLogin $e) {
      Yii::app()->facebook->doLogin();
      return false;
    }
    
    return true;
  }
  
  public function getUserOrRedirect($to=array('site/index')) {
    if ($this->getUser(true)) return true;
    $this->redirect($to);
    return false;
  }

  public function afterAction() {
    $count = Yii::app()->redis->conn->count;
    Yii::log("Queries: $count", "info");
  }
  
  public function getSession() {
    $sessionID = Yii::app()->session->sessionID;
    return RedisSession::ensure($sessionID);
  }
  
}