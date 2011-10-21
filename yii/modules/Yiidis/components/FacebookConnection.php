<?php

class FacebookNeedsLogin extends Exception {}

class FacebookConnection extends CApplicationComponent {

  public $params;
  public $conn;
  public $appUserClass;
  public $breakFrame = false;

  public $_session;

  public function init() {
    $this->conn = new Facebook($this->params['connection']);
    parent::init();
  }

  public function getSession() {
    if (!$this->_session) {
      $sessionID = Yii::app()->session->sessionID;
      $this->_session = RedisSession::ensure($sessionID);
    }
    return $this->_session;
  }

  public function getUser($login=true) {

    $session = $this->getSession();

    if ($session->userID) {
      return $session->getUser();
    }

    if (!$login) return null;

    $fbUser = $this->conn->getUser();

    if (!$fbUser) {
      throw new FacebookNeedsLogin("No facebook user");
    }

    try {
      $info = $this->conn->api('/me');
      $perms = $this->conn->api('/me/permissions');
      $friends = $this->conn->api('/me/friends');
      $friends = $friends['data'];
    } catch (FacebookApiException $e) {
      throw new FacebookNeedsLogin("No facebook user");
    }

    $userKey = 'fb:' . $info['id'];
    $user = FacebookUser::ensure($userKey);

    $user->name = $info['name'];
    $user->id = $info['id'];
    $user->perms = array_keys($perms['data'][0]);
    $user->put();

    $profile = FacebookProfile::ensure($userKey);
    $profile->info = $info;
    $profile->friends = $friends;
    $profile->updateFriendCache();
    $profile->put();

    $session->userID = $userKey;
    $session->put();

    return $user;
  }

  public function doLogin($scope=array()) {
    $session = $this->getSession();
    $session->clearUser();

    $app = Yii::app();

    if ($this->breakFrame) {
      $app->request->redirect($app->createUrl("yiidis/facebook/login", array('scope'=>implode(",", $scope))));
    } else {
      $app->request->redirect($this->getLoginUrl($scope));
    }
  }

  public function getLoginUrl($scope=array(), $appRedirect=false) {
      $app = Yii::app();
      $request = $app->getRequest();
      
      if ($appRedirect) {
        $cb = $this->params['appUrl'];
      } else {
        $cb = $request->getBaseUrl() . $app->createUrl("yiidis/facebook/afterLogin");
      }

      $strScope = implode(",", $scope);
      return $this->conn->getLoginUrl(array('redirect_uri'=>$cb, 'scope'=>$strScope));
  }

  public function doLogout() {
    $session = $this->getSession();
    $session->clearUser();

    $app = Yii::app();
    $request = $app->getRequest();
    $cb = $request->getBaseUrl() . $app->createUrl("yiidis/facebook/afterLogout");
    $url = $this->conn->getLoginUrl(array('redirect_uri'=>$cb));
    $request->redirect($url);
  }

}
