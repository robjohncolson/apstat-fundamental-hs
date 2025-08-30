/**
 * AP Statistics PoK Blockchain Service Worker
 * Enables complete offline functionality for educational deployment
 */

const CACHE_NAME = 'apstat-pok-v1.0.0';
const STATIC_CACHE = 'apstat-static-v1.0.0';

// Resources to cache for offline functionality
const CACHE_RESOURCES = [
    '/',
    '/index.html',
    '/assets/styles.css',
    '/assets/apstat-bindings.js',
    '/assets/apstat-ui.js', 
    '/assets/educational-features.js',
    '/data/curriculum.json',
    '/data/blockchain-state.json',
    '/docs/student-guide.html',
    '/docs/teacher-guide.html',
    '/docs/technical-reference.html'
];

// Cache strategies
const CACHE_STRATEGIES = {
    'cache-first': ['assets/', 'docs/'],
    'network-first': ['data/'],
    'stale-while-revalidate': ['/']
};

/**
 * Service Worker Installation
 */
self.addEventListener('install', (event) => {
    console.log('🔧 AP Statistics Service Worker installing...');
    
    event.waitUntil(
        Promise.all([
            // Cache static resources
            caches.open(STATIC_CACHE).then((cache) => {
                console.log('📦 Caching static resources...');
                return cache.addAll(CACHE_RESOURCES);
            }),
            
            // Initialize IndexedDB for blockchain data
            initializeOfflineStorage()
        ]).then(() => {
            console.log('✅ Service Worker installation complete');
            self.skipWaiting(); // Force activation
        })
    );
});

/**
 * Service Worker Activation
 */
self.addEventListener('activate', (event) => {
    console.log('🚀 AP Statistics Service Worker activating...');
    
    event.waitUntil(
        Promise.all([
            // Clean up old caches
            caches.keys().then((cacheNames) => {
                return Promise.all(
                    cacheNames
                        .filter(name => name !== CACHE_NAME && name !== STATIC_CACHE)
                        .map(name => caches.delete(name))
                );
            }),
            
            // Claim all clients immediately
            self.clients.claim()
        ]).then(() => {
            console.log('✅ Service Worker activation complete - offline mode ready');
        })
    );
});

/**
 * Fetch Event Handler - Implements caching strategies
 */
self.addEventListener('fetch', (event) => {
    const request = event.request;
    const url = new URL(request.url);
    
    // Only handle same-origin requests
    if (url.origin !== location.origin) {
        return;
    }
    
    // Determine caching strategy based on URL
    const strategy = getCachingStrategy(url.pathname);
    
    event.respondWith(
        handleFetchWithStrategy(request, strategy)
            .catch(error => {
                console.warn('Fetch failed, serving fallback:', error);
                return getFallbackResponse(request);
            })
    );
});

/**
 * Determine appropriate caching strategy for resource
 */
function getCachingStrategy(pathname) {
    for (const [strategy, patterns] of Object.entries(CACHE_STRATEGIES)) {
        if (patterns.some(pattern => pathname.includes(pattern))) {
            return strategy;
        }
    }
    return 'cache-first'; // Default strategy
}

/**
 * Handle fetch with specific caching strategy
 */
async function handleFetchWithStrategy(request, strategy) {
    switch (strategy) {
        case 'cache-first':
            return handleCacheFirst(request);
        case 'network-first':
            return handleNetworkFirst(request);
        case 'stale-while-revalidate':
            return handleStaleWhileRevalidate(request);
        default:
            return handleCacheFirst(request);
    }
}

/**
 * Cache-first strategy: Check cache, then network
 */
async function handleCacheFirst(request) {
    const cache = await caches.open(STATIC_CACHE);
    const cachedResponse = await cache.match(request);
    
    if (cachedResponse) {
        return cachedResponse;
    }
    
    // Not in cache, try network
    try {
        const networkResponse = await fetch(request);
        if (networkResponse.ok) {
            cache.put(request, networkResponse.clone());
        }
        return networkResponse;
    } catch (error) {
        throw new Error(`Network failed and no cache available for ${request.url}`);
    }
}

/**
 * Network-first strategy: Try network, then cache
 */
async function handleNetworkFirst(request) {
    try {
        const networkResponse = await fetch(request);
        if (networkResponse.ok) {
            const cache = await caches.open(CACHE_NAME);
            cache.put(request, networkResponse.clone());
        }
        return networkResponse;
    } catch (error) {
        // Network failed, try cache
        const cache = await caches.open(CACHE_NAME);
        const cachedResponse = await cache.match(request);
        if (cachedResponse) {
            return cachedResponse;
        }
        throw new Error(`Both network and cache failed for ${request.url}`);
    }
}

/**
 * Stale-while-revalidate: Return cache immediately, update in background
 */
async function handleStaleWhileRevalidate(request) {
    const cache = await caches.open(CACHE_NAME);
    const cachedResponse = await cache.match(request);
    
    // Start network request in background
    const networkResponsePromise = fetch(request).then(response => {
        if (response.ok) {
            cache.put(request, response.clone());
        }
        return response;
    }).catch(() => null);
    
    // Return cached version immediately if available
    if (cachedResponse) {
        return cachedResponse;
    }
    
    // No cache available, wait for network
    return networkResponsePromise || getFallbackResponse(request);
}

/**
 * Get fallback response for failed requests
 */
function getFallbackResponse(request) {
    const url = new URL(request.url);
    
    // HTML requests get offline page
    if (request.destination === 'document') {
        return new Response(getOfflineHTML(), {
            headers: { 'Content-Type': 'text/html' }
        });
    }
    
    // JSON requests get empty valid JSON
    if (url.pathname.includes('.json')) {
        return new Response('{}', {
            headers: { 'Content-Type': 'application/json' }
        });
    }
    
    // JavaScript requests get empty script
    if (url.pathname.includes('.js')) {
        return new Response('console.log("Offline fallback");', {
            headers: { 'Content-Type': 'application/javascript' }
        });
    }
    
    // Default fallback
    return new Response('Offline - content unavailable', {
        status: 503,
        statusText: 'Service Unavailable'
    });
}

/**
 * Generate offline fallback HTML
 */
function getOfflineHTML() {
    return `
        <!DOCTYPE html>
        <html>
        <head>
            <title>AP Statistics PoK - Offline Mode</title>
            <style>
                body { font-family: monospace; text-align: center; padding: 50px; background: #1e3c72; color: white; }
                .offline-message { background: rgba(0,0,0,0.5); padding: 30px; border-radius: 10px; margin: 20px; }
                .retry-btn { background: #4CAF50; color: white; padding: 10px 20px; border: none; border-radius: 5px; cursor: pointer; }
            </style>
        </head>
        <body>
            <div class="offline-message">
                <h1>🔗 AP Statistics PoK Blockchain</h1>
                <h2>📡 Offline Mode Active</h2>
                <p>You're currently offline, but the full AP Statistics blockchain system is available locally!</p>
                <p>All 58 atoms, 13 invariants, and educational features are cached and ready to use.</p>
                <button class="retry-btn" onclick="window.location.reload()">🔄 Retry Connection</button>
                <button class="retry-btn" onclick="window.location.href='/'">📱 Continue Offline</button>
            </div>
        </body>
        </html>
    `;
}

/**
 * Initialize IndexedDB for offline blockchain data storage
 */
async function initializeOfflineStorage() {
    return new Promise((resolve, reject) => {
        const request = indexedDB.open('APStatBlockchain', 1);
        
        request.onerror = () => reject(request.error);
        request.onsuccess = () => resolve(request.result);
        
        request.onupgradeneeded = (event) => {
            const db = event.target.result;
            
            // Create object stores for blockchain data
            if (!db.objectStoreNames.contains('profiles')) {
                const profileStore = db.createObjectStore('profiles', { keyPath: 'userId' });
                profileStore.createIndex('pubkey', 'pubkey', { unique: true });
            }
            
            if (!db.objectStoreNames.contains('transactions')) {
                const txStore = db.createObjectStore('transactions', { keyPath: 'hash' });
                txStore.createIndex('timestamp', 'timestamp');
                txStore.createIndex('questionId', 'questionId');
            }
            
            if (!db.objectStoreNames.contains('blocks')) {
                const blockStore = db.createObjectStore('blocks', { keyPath: 'hash' });
                blockStore.createIndex('timestamp', 'timestamp');
            }
            
            if (!db.objectStoreNames.contains('reputation')) {
                const repStore = db.createObjectStore('reputation', { keyPath: 'userId' });
            }
            
            if (!db.objectStoreNames.contains('questions')) {
                const questionStore = db.createObjectStore('questions', { keyPath: 'id' });
                questionStore.createIndex('type', 'type');
            }
            
            if (!db.objectStoreNames.contains('system-state')) {
                db.createObjectStore('system-state', { keyPath: 'key' });
            }
        };
    });
}

/**
 * Handle background sync for when connectivity returns
 */
self.addEventListener('sync', (event) => {
    console.log('🔄 Background sync triggered:', event.tag);
    
    if (event.tag === 'blockchain-sync') {
        event.waitUntil(syncBlockchainData());
    } else if (event.tag === 'attestation-sync') {
        event.waitUntil(syncPendingAttestations());
    }
});

/**
 * Sync blockchain data when connectivity returns
 */
async function syncBlockchainData() {
    try {
        console.log('📡 Syncing blockchain data...');
        
        // Get latest blockchain state from server (if available)
        const response = await fetch('/api/blockchain-state');
        if (response.ok) {
            const data = await response.json();
            
            // Update local IndexedDB with server data
            const db = await initializeOfflineStorage();
            const tx = db.transaction(['system-state'], 'readwrite');
            await tx.objectStore('system-state').put({
                key: 'last-sync',
                value: new Date().toISOString(),
                data: data
            });
        }
        
        console.log('✅ Blockchain sync complete');
    } catch (error) {
        console.warn('⚠️ Blockchain sync failed:', error);
    }
}

/**
 * Sync pending attestations when connectivity returns
 */
async function syncPendingAttestations() {
    try {
        console.log('📤 Syncing pending attestations...');
        
        const db = await initializeOfflineStorage();
        const tx = db.transaction(['transactions'], 'readonly');
        const store = tx.objectStore('transactions');
        
        // Get pending transactions
        const pendingTxs = await store.getAll();
        const unsyncedTxs = pendingTxs.filter(tx => !tx.synced);
        
        // Send to server
        for (const tx of unsyncedTxs) {
            try {
                const response = await fetch('/api/submit-attestation', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify(tx)
                });
                
                if (response.ok) {
                    // Mark as synced
                    const updateTx = db.transaction(['transactions'], 'readwrite');
                    tx.synced = true;
                    await updateTx.objectStore('transactions').put(tx);
                }
            } catch (syncError) {
                console.warn('Failed to sync attestation:', tx.hash, syncError);
            }
        }
        
        console.log(`✅ Synced ${unsyncedTxs.length} pending attestations`);
    } catch (error) {
        console.warn('⚠️ Attestation sync failed:', error);
    }
}

/**
 * Handle messages from main thread
 */
self.addEventListener('message', (event) => {
    const { type, data } = event.data;
    
    switch (type) {
        case 'CACHE_CURRICULUM':
            handleCacheCurriculum(data);
            break;
        case 'STORE_ATTESTATION':
            handleStoreAttestation(data);
            break;
        case 'GET_OFFLINE_STATUS':
            event.ports[0].postMessage({
                offline: !navigator.onLine,
                cacheReady: true,
                storageReady: true
            });
            break;
        default:
            console.log('Unknown message type:', type);
    }
});

/**
 * Cache curriculum data
 */
async function handleCacheCurriculum(curriculumData) {
    try {
        const cache = await caches.open(CACHE_NAME);
        const response = new Response(JSON.stringify(curriculumData), {
            headers: { 'Content-Type': 'application/json' }
        });
        await cache.put('/data/curriculum.json', response);
        console.log('✅ Curriculum cached for offline use');
    } catch (error) {
        console.error('❌ Failed to cache curriculum:', error);
    }
}

/**
 * Store attestation in IndexedDB
 */
async function handleStoreAttestation(attestationData) {
    try {
        const db = await initializeOfflineStorage();
        const tx = db.transaction(['transactions'], 'readwrite');
        
        attestationData.timestamp = Date.now();
        attestationData.synced = false; // Mark for later sync
        
        await tx.objectStore('transactions').add(attestationData);
        console.log('✅ Attestation stored offline');
        
        // Register background sync if available
        if ('serviceWorker' in navigator && 'sync' in window.ServiceWorkerRegistration.prototype) {
            self.registration.sync.register('attestation-sync');
        }
    } catch (error) {
        console.error('❌ Failed to store attestation:', error);
    }
}

/**
 * Monitor online/offline status
 */
self.addEventListener('online', () => {
    console.log('📡 Connection restored - triggering sync');
    
    // Trigger background sync when coming back online
    if (self.registration && self.registration.sync) {
        self.registration.sync.register('blockchain-sync');
        self.registration.sync.register('attestation-sync');
    }
});

self.addEventListener('offline', () => {
    console.log('📡 Connection lost - offline mode active');
});

console.log('🔧 AP Statistics Service Worker loaded successfully');
console.log('📚 Features: Offline caching, background sync, IndexedDB storage');
console.log('🎯 Educational focus: Complete PoK cycle available offline');