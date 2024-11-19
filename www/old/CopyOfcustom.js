


$(document).ready(function() {
  $(document).on('click', '#logo_img', function() {
    console.log("Image clicked");
    Shiny.setInputValue('image_clicked', Math.random());
  });
});







$(document).ready(function() {
document.getElementById('go_to_login').onclick = function() {
    console.log("goto login clicked");
    Shiny.setInputValue('go_to_login_page', Math.random());
    
  };
});







// JavaScript in custom.js

let ignorePopState = false; // Flag to prevent infinite loops

document.addEventListener('DOMContentLoaded', function() {
  console.log('DOM fully loaded and parsed');
  initializeApp();
});

function initializeApp() {
  console.log('Initializing app...');
  // Check if the URL has no query string when the app first loads
  if (!window.location.search) {
    console.log('No query string found, setting default to ?HOME');
    // Set the URL to ?HOME on first load
    history.replaceState(null, '', '?HOME');
    Shiny.setInputValue('current_tab', 'HOME', {priority: 'event'});
  } else {
    // Initialize Shiny input based on existing URL query string
    var initialPath = window.location.search.split('?')[1];  // Get URL params after '?'
    console.log('Query string found, setting current_tab to:', initialPath);
    Shiny.setInputValue('current_tab', initialPath, {priority: 'event'});
  }
}

// Function to update URL dynamically
Shiny.addCustomMessageHandler('updateURL', function(message) {
  console.log('Updating URL to:', message);
  if (window.location.search !== '?' + message) { // Only push new state if URL is different
    ignorePopState = true;  // Set flag to true before pushing new state
    history.pushState(null, '', '?' + message);
    ignorePopState = false; // Reset flag after state change
  }
});

// Listen for tab changes triggered by user click

$(document).on('shown.bs.tab', 'a[data-toggle="tab"]', function (e) {
  var tabId = $(e.target).attr('data-value');
  console.log('Tab changed to:', tabId);
  Shiny.setInputValue('current_tab', tabId, {priority: 'event'});

  var menu = document.querySelector('.dropdown-menu.show');  // Find the open dropdown menu
  if (menu) {
    menu.classList.remove('show');  // Remove 'show' class to close it
  }
});


// Listen for back and forward button events
window.addEventListener('popstate', function(event) {
  if (!ignorePopState) {
    var path = window.location.search.split('?')[1];  // Get URL params after '?'
    console.log('Back/Forward navigation detected, setting current_tab to:', path);
    if (path) {
      Shiny.setInputValue('current_tab', path, {priority: 'event'});
    } else {
      Shiny.setInputValue('current_tab', 'HOME', {priority: 'event'}); // Default to HOME if no path
    }
  }
});









// is the full screen being closed?




$(document).on('shiny:connected', function() {
  // Select the target element
  const targetElement = document.getElementById('top_of_estimation_tab-card_module-my_card');

  // Create a MutationObserver to watch for changes in attributes
  const observer = new MutationObserver((mutationsList) => {
    mutationsList.forEach((mutation) => {
      if (
        mutation.type === 'attributes' &&
        mutation.attributeName === 'data-full-screen'
      ) {
        const isFullScreen = targetElement.getAttribute('data-full-screen');
        
        // Check if full screen is closed (data-full-screen changes to "false")
        if (isFullScreen === 'false') {
          // Send event to Shiny
          Shiny.setInputValue('full_screen_closed', true, {priority: 'event'});
        }
      }
    });
  });

  // Configure the observer to watch for attribute changes
  observer.observe(targetElement, { attributes: true });
});







// is hometeam, middle or away team clicked?

$(document).ready(function() {
  $(document).on('click', '#top_of_estimation_tab-in_main_page-logo_ht', function() {
    console.log("Home clicked");
    Shiny.setInputValue('div_clicked_home_team', Math.random());
  });
});
$(document).ready(function() {
  $(document).on('click', '#top_of_estimation_tab-card_module-in_main_card-logo_ht', function() {
    console.log("Home clicked");
    Shiny.setInputValue('div_clicked_home_team', Math.random());
  });
});

$(document).ready(function() {
  $(document).on('click', '#top_of_estimation_tab-in_main_page-logo_at', function() {
    console.log("Away clicked");
    Shiny.setInputValue('div_clicked_away_team', Math.random());
  });
});
$(document).ready(function() {
  $(document).on('click', '#top_of_estimation_tab-card_module-in_main_card-logo_at', function() {
    console.log("Away clicked");
    Shiny.setInputValue('div_clicked_away_team', Math.random());
  });
});

$(document).ready(function() {
  $(document).on('click', '#top_of_estimation_tab-in_main_page-clicked_result_col', function() {
    console.log("result col clicked");
    Shiny.setInputValue('div_clicked_result_col', Math.random());
  });
});



/* enter to log in */

$(document).ready(function() {
  // Listen for "Enter" key on the username or password fields
  $(document).on('keydown', function(e) {
    if (e.key === 'Enter') {
      
      console.log("Enter Button Clicked");
      
      // Check if focus is on either the username or password input
      if ($('#login_page-username').is(':focus') || $('#login_page-password').is(':focus')) {
        
        console.log("focus was correct");
        
        // Trigger the login button click
        $('#login_page-login_btn').click();
        
        setTimeout(function() {
          $('#login_page-login_btn').click();
        }, 100); // 100 ms delay
        
        
      }
    }
  });
});


// Function to toggle full screen for the card via Shiny message handler
Shiny.addCustomMessageHandler('toggleCardFullscreen', function(card_id) {
  console.log("Received toggleCardFullscreen message for card ID:", card_id);
  var card = document.getElementById(card_id);
  if (card) {
    var fullscreenButton = card.querySelector('.bslib-full-screen-enter');
    if (fullscreenButton) {
      console.log("Full-screen button found and clicked");
      fullscreenButton.click();

      // Return focus to the main content (e.g., body) to restore tab navigation
      setTimeout(function() {
        document.body.focus(); // Set focus back to the main content
      }, 0);

    } else {
      console.warn("Full-screen button not found within card");
    }
  } else {
    console.warn("Card element not found with ID:", card_id);
  }
});



