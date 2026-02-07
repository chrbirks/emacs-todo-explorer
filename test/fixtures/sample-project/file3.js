// file3.js - Sample JavaScript file

function fetchUser(id) {
  // FIXME: Handle 404 responses
  return fetch(`/api/users/${id}`);
}

// REVIEW: Should this be async?
function processQueue(queue) {
  // XXX: Race condition possible here
  return queue.map(item => item.process());
}
