var PwdEntry = (function () {
  // these must be set by go()
  var the_password = false;
  var the_session_key = false;
  var the_timestamp = false;
  var the_userid = false;
  var the_record_data_url = false;

  var t1 = 'Do you remember your password from last time? Type as much' +
      ' of it as you remember here, then press return. (If this is your' +
      ' first time, just hit return.)';

  var t2bad = 'Here\'s the correct password. Type it in the box, and then' +
      ' press return:';

  var t2good = 'You got it! Okay, this will be easy. Type the password in' +
      ' again, and then press return:';

  var t3 = 'Good! Now type it again:';

  var t4 = 'Last time with help, type it again (and try to remember it, because ' +
      'you\'re going to try one more time without help)';

  var t5 = 'Okay, last time, see if you can remember it. Type as much as you' +
        ' remember, then hit return:';

  var t6bad = 'Good try. Many thanks for your help! You can close this page now.';

  var t6good = 'Awesome! Many thanks for your help. You can close this page now.';

  var stepTexts = [
    [t1, t1],
    [t2bad, t2good],
    [t3, t3],
    [t4, t4],
    [t5, t5],
    [t6bad, t6good]
  ];

  // states to be sent to the server.
  var statequeue = [];
  // the number of states generated:
  var states_queued = 0;
  // the number of states successfully sent to the server.
  var states_sent = 0;

  var STATE_QUEUE_SIZE = 50;

  // the size of the green/red boxes:
  var CHAR_METER_BOX_SIDE = 20;

  // the total number of password entry boxes:
  var NUM_PWD_BOXES = 5;

  // a table of functions for starting given steps.
  // the indirection of the table is necessary to
  // avoid nesting the the continuations
  // in a way that would be really hard to read.
  var stepStarters = [
    startStepOne,
    startStepTwo,
    stepMaker(3),
    stepMaker(4),
    startStepFive
  ];

  // start the program running:
  function go (uid, sessionkey, trainingstr, recorddataurl) {
    the_userid = uid;
    the_session_key = sessionkey;
    the_password = trainingstr;
    the_record_data_url = recorddataurl;
    the_timestamp = Date.now();
    hideEverything();
    goKont();
  }

  // continue after session initialization
  function goKont () {
    setupAllMeters();
    startStep(1);
  }

  // hide all of the password entry boxes and the meters and the
  // password display
  function hideEverything () {
    var i;
    for (i = 1; i <= NUM_PWD_BOXES; i++) {
      $('#pwdentry' + i).hide();
      $('#pwd' + i + 'meter').hide();
    }
    $('#pwddisplay').hide();
  }

  // set up all of the meters:
  function setupAllMeters () {
    var i;
    for (i = 1; i <= NUM_PWD_BOXES; i++) {
      setupMeter(i);
    }
  }

  // start the specified step:
  function startStep (step) {
    stepStarters[step - 1]();
  }

  // start the first step
  function startStepOne () {
    enableBox(1, true, boxKey(1, false), startStepTwo);
    $('#pwd1meter').hide();
  }

  // start the second step
  function startStepTwo () {
    $('#pwddisplay').text(the_password);
    $('#pwddisplay').show();
    var success = boxMatches(1);
    enableBox(2, success, boxKey(2, true), maybeStartStep(3));
  }

  // do the contents of the box match the password?
  function boxMatches (step) {
    return $('#pwdentry' + step)[0].value === the_password;
  }

  // start step 3 & 4
  function stepMaker (step) {
    return function () {
      tearDownMeter(step - 1);
      enableBox(step, true, boxKey(step, true), maybeStartStep(step + 1));
    };
  }

  // start the last step
  function startStepFive () {
    hideEverything();
    enableBox(5, true, boxKey(5, false), thanksAndGoodbye);
    $('#pwd5meter').hide();
  }

  function thanksAndGoodbye () {
    $('#pwdentry5').attr('readonly', 'true');
    $('#pwddisplay').show();
    $('#pwd5meter').show();
    var success = boxMatches(5);
    flushUserTextQueue(function () {
      setInstructions(6, success);
    });
  }

  // only go to the next step if the text is correct:
  function maybeStartStep (step) {
    return function () {
      if ($('#pwdentry' + (step - 1))[0].value === the_password) {
        stepStarters[step - 1]();
      } else {
        window.alert('incorrect. This popup is too annoying.');
      }
    };
  }

  // set the instructions to the specified step. Note that steps
  // are numbered starting at 1
  function setInstructions (step, success) {
    var i;
    for (i = 0; i < stepTexts.length; i++) {
      $('#instructions' + (i + 1)).hide();
    }
    $('#instructions' + step).show();
    $('#instructions' + step).text(stepTexts[step - 1][(success ? 1 : 0)]);
  }

  // canvas draw functions
  function setupMeter (step) {
    var canvases = _.map(_.range(0, the_password.length),
                         createPwdCharMeterHTML(step));
    $('#pwd' + step + 'meter').html(canvases.join(' '));
  }

  function tearDownMeter (step) {
    $('#pwd' + step + 'meter').html('');
  }

  // create a small canvas to reflect the state of a particular
  // password entry character (curried)
  function createPwdCharMeterHTML (pwd) {
    return function (idx) {
      return '<canvas width="' + CHAR_METER_BOX_SIDE + '" height = "' +
          CHAR_METER_BOX_SIDE + '" class="pwdMeterCharBox" id="pmcb' + pwd + 'Z' +
          idx + '"></canvas>';
    };
  }

  // given the step number and whether to lock the text box when
  // the user gets the right string, produce a callback function
  // that can be used on every change
  function boxKey (step, locking) {
    return function () {
      var i;
      var usertext = $('#pwdentry' + step)[0].value;
      logUserText(step, usertext);
      for (i = 0; i < the_password.length; i++) {
        var canvas = $('#pmcb' + step + 'Z' + i)[0];
        if (canvas === undefined) {
          window.alert('Yikes! Internal error');
        }
        var ctx = canvas.getContext('2d');

        if (i >= usertext.length) {
          ctx.fillStyle = 'white';
        } else if (usertext[i] === the_password[i]) {
          ctx.fillStyle = 'green';
        } else {
          ctx.fillStyle = 'red';
        }
        ctx.fillRect(0, 0, CHAR_METER_BOX_SIDE, CHAR_METER_BOX_SIDE);
      }
      if (locking && (usertext === the_password)) {
        $('#pwdentry' + step).attr('readonly', 'true');
      }
    };
  }

  // given a step number, whether the previous password entry was correct,
  // a function to call on every box content change, and a continuation,
  // show and focus the given password box, change the instructions, and
  // set the continuation to the given function. make the previous box
  // unreadable (if there is one).
  function enableBox (step, success, keyfun, kont) {
    if (step > 1) {
      var prevStep = step - 1;
      $('#pwdentry' + prevStep).attr('readonly', 'true');
    }
    var input = $('#pwdentry' + step);
    setInstructions(step, success);
    input.attr('value', '');
    input.show();
    input.focus();
    input.on('input', keyfun);
    input.on('change', kont);
    $('#pwd' + step + 'meter').show();
  }

  // PHONE HOME FUNCTIONS

  // record the current state of the password box in the
  // data queue. if the size of the queue is too large,
  // asynchronously send the current data to the server.
  function logUserText (step, usertext) {
    var timediff = Date.now() - the_timestamp;
    statequeue.push({t: timediff, n: step, p: usertext});
    states_queued += 1;
    if (statequeue.length >= STATE_QUEUE_SIZE) {
      flushUserTextQueue(function () { return 0; });
    }
  }

  // take all of the states in the queue and send them.
  // run kont when finished.
  function flushUserTextQueue (kont) {
    // no race conditions possible in javascript except
    // between function calls.
    var to_send = statequeue;
    statequeue = [];
    var packet = {userid: the_userid,
                  sessionkey: the_session_key,
                  data: to_send};
    $.post(RECORD_DATA_URL,
           JSON.stringify(packet),
           function () {
             states_sent += to_send.length;
             kont();
           });
  }

  return {go: go};
})();
