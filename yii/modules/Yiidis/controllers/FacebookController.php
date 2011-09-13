<?php

class FacebookController extends YiidisController {

  public $requireLogin = false;

  public function actions() {
    return array();
  }

  public function actionLogin($scope) {
    $scope = explode(',', $scope);
    $url = Yii::app()->facebook->getLoginUrl($scope, true);
    $this->renderPartial('jsredirect', array('url'=>$url));
  }

  public function actionCleanupUrl() {
    $url = Yii::app()->facebook->params['appUrl'];
    $this->renderPartial('jsredirect', array('url'=>$url));
  }

  public function actionAfterLogin() {
    try {
      if (Yii::app()->facebook->getUser()) {
        Yii::log("Facebook login successful", "info");
      } else {
        Yii::log("Facebook login failed", "info");
      }
    } catch (Exception $e) {
        Yii::log("Facebook login error", "info");
        Yii::log(CVarDumper::dumpAsString($e), "info");
    }
    Yii::app()->request->redirect("/");
  }

  public function actionAfterLogout() {
    Yii::log("Facebook logout successful", "info");
    Yii::app()->request->redirect("/");
  }

}