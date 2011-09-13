<?php

class YiidisController extends CController {

  public $requireLogin = true;
  public $user;
  
  public function beforeAction() {
    if (strstr(Yii::app()->request->getUrlReferrer(), 'facebook') && isset($_GET['state'])) {
      $this->getUser(true);
      $this->redirect(array('yiidis/facebook/cleanupUrl'));
    }
    return $this->getUser();
  }

  public function getUser($login = null, $scope=array()) {
    if ($login === null) $login = $this->requireLogin;
    try {
      $this->user = Yii::app()->facebook->getUser($login);
    } catch (FacebookNeedsLogin $e) {
      Yii::app()->facebook->doLogin($scope);
      return false;
    }

    foreach ($scope as $perm) {
      if (!in_array($perm, $this->user->perms)) {
        Yii::app()->facebook->doLogin($scope);
        return false;
      }
    }
    
    return true;
  }
  
  public function getUserOrRedirect($to=array('site/index'), $scope=array()) {
    if ($this->getUser(true, $scope)) return true;
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