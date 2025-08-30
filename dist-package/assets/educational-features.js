/**
 * AP Statistics Educational Features Module
 * Interactive learning aids for invariant explanations and statistics visualization
 */

class EducationalFeatures {
    constructor() {
        this.invariants = this.getInvariantDefinitions();
        this.charts = {};
        this.tutorialStep = 0;
        this.setupModal();
    }

    /**
     * Define all 13 invariants with educational explanations
     */
    getInvariantDefinitions() {
        return {
            1: {
                name: "Identity Integrity",
                technical: "∀ transaction t: t.attesterPubkey ∈ {p.pubkey | p ∈ profiles}",
                explanation: "Every attestation must come from a registered user. This prevents fake testimonies and ensures accountability in the system.",
                example: "Like requiring a student ID to vote in class - only enrolled students can participate in consensus.",
                importance: "Prevents Sybil attacks where one person creates multiple fake identities."
            },
            2: {
                name: "Progressive Quorum",
                technical: "∀ block b: |b.attestations| ≥ progressiveQuorum(b.convergence)",
                explanation: "The number of attestations needed depends on how much agreement there is. High agreement needs fewer votes, low agreement needs more.",
                example: "If 90% of students agree on an answer, we need only 3 confirmations. If only 60% agree, we need 5 confirmations.",
                importance: "Balances efficiency with security - prevents premature consensus on controversial questions."
            },
            3: {
                name: "Confidence Weighting",
                technical: "∀ profile p: p.reputationScore includes confidence weighting",
                explanation: "Students who are more confident in their answers (1-5 scale) get weighted more heavily if they're correct.",
                example: "A student confident at level 5 who gets it right gains more reputation than someone confident at level 2.",
                importance: "Encourages honest self-assessment and rewards students who know when they know something."
            },
            4: {
                name: "Hash Validation",
                technical: "∀ MCQ answer a: sha256Hash(a.choice) = a.answerHash",
                explanation: "Multiple choice answers are encrypted (hashed) until consensus forms, preventing early influence on other students.",
                example: "Your answer 'B' becomes a long random string until enough people vote, then it's revealed as 'B'.",
                importance: "Prevents bandwagon effects where students copy early answers instead of thinking independently."
            },
            5: {
                name: "FRQ Scoring Bounds",
                technical: "∀ FRQ response r: 1.0 ≤ scoreFRQ(r, rubric) ≤ 5.0",
                explanation: "Free response questions are scored on a 1-5 scale based on rubric criteria.",
                example: "A well-structured answer showing work gets a 5, a partially correct answer gets a 3, a guess gets a 1.",
                importance: "Standardizes grading across peer reviewers and prevents extreme scores."
            },
            6: {
                name: "Temporal Ordering",
                technical: "∀ timestamp t: t.current > t.previous",
                explanation: "All events must happen in chronological order - no time travel allowed!",
                example: "You can't submit an attestation dated yesterday for a question asked today.",
                importance: "Prevents backdating attacks and ensures proper sequence of learning events."
            },
            7: {
                name: "Convergence Calculation",
                technical: "MCQ: max_count/total, FRQ: max(0, 1 - stdDev/mean)",
                explanation: "Measures how much students agree. For multiple choice, it's the percentage choosing the most popular answer. For free response, it's based on how similar the scores are.",
                example: "If 8 out of 10 students choose 'C', convergence is 80%. If FRQ scores are [4,4,4,5], convergence is high.",
                importance: "Determines when we have enough agreement to move forward - the core of emergent consensus."
            },
            8: {
                name: "Rate Limiting",
                technical: "∀ user u, question q: timeSinceLastAttestation(u, q) > 30 days",
                explanation: "Students can only attest to the same question once every 30 days to prevent spam and gaming.",
                example: "Once you answer 'What is the mean of [1,2,3,4,5]?', you can't answer it again for a month.",
                importance: "Prevents students from repeatedly changing their answers to manipulate consensus."
            },
            9: {
                name: "Outlier Detection",
                technical: "∀ attestation a: detectOutliers([a]) flagged for review",
                explanation: "The system automatically identifies unusual voting patterns that might indicate gaming or errors.",
                example: "If one student always votes opposite to everyone else, they get flagged for review.",
                importance: "Maintains system integrity by catching cheating attempts or systematic errors."
            },
            10: {
                name: "Cycle Stability",
                technical: "System graph is DAG except reputation feedback loop",
                explanation: "Information flows in one direction (questions → attestations → reputation) with only reputation feeding back to future attestations.",
                example: "Questions lead to answers, answers affect reputation, but questions don't depend on answers.",
                importance: "Prevents circular dependencies that could crash the system or create logical contradictions."
            },
            11: {
                name: "Persistence Integrity",
                technical: "∀ state s: loadState(saveState(s)) = s",
                explanation: "Whatever you save to the blockchain can be perfectly restored later - no data corruption.",
                example: "If you save your progress and reload the page, everything should be exactly as you left it.",
                importance: "Ensures student work is never lost and the system maintains consistency across sessions."
            },
            12: {
                name: "Atomicity",
                technical: "∀ atom a ∈ S: independent(a) ⇒ testable(a)",
                explanation: "Every component of the system can be tested independently, like building blocks.",
                example: "We can test the reputation calculator without needing the entire blockchain running.",
                importance: "Makes the system maintainable and helps identify problems quickly during development."
            },
            13: {
                name: "UI Safety",
                technical: "∀ state s: renderState(s) ≠ null ∧ validView(s.currentView)",
                explanation: "The interface always shows something meaningful and never crashes or shows blank screens.",
                example: "Even if there's an error, you'll see an error message instead of a blank page.",
                importance: "Ensures students can always interact with the system, even when things go wrong."
            }
        };
    }

    /**
     * Setup modal for invariant explanations
     */
    setupModal() {
        const modal = document.createElement('div');
        modal.id = 'invariant-modal';
        modal.className = 'modal';
        modal.innerHTML = `
            <div class="modal-content">
                <span class="close" onclick="closeInvariantModal()">&times;</span>
                <h2 id="modal-title"></h2>
                <div id="modal-body"></div>
                <div class="modal-tabs">
                    <button class="tab-button active" onclick="showTab('explanation')">Simple Explanation</button>
                    <button class="tab-button" onclick="showTab('technical')">Technical Details</button>
                    <button class="tab-button" onclick="showTab('example')">Examples</button>
                </div>
            </div>
        `;
        document.body.appendChild(modal);
    }

    /**
     * Show invariant explanation modal
     */
    showInvariant(id) {
        const invariant = this.invariants[id];
        if (!invariant) return;

        document.getElementById('modal-title').textContent = `Invariant ${id}: ${invariant.name}`;
        
        const modalBody = document.getElementById('modal-body');
        modalBody.innerHTML = `
            <div class="tab-content active" id="explanation-tab">
                <h3>What does this mean?</h3>
                <p>${invariant.explanation}</p>
                <div class="importance-box">
                    <h4>Why is this important?</h4>
                    <p>${invariant.importance}</p>
                </div>
            </div>
            <div class="tab-content" id="technical-tab">
                <h3>Mathematical Definition</h3>
                <code>${invariant.technical}</code>
                <p>This mathematical notation precisely defines the rule that must always hold true in the system.</p>
            </div>
            <div class="tab-content" id="example-tab">
                <h3>Real-world Example</h3>
                <div class="example-box">
                    <p>${invariant.example}</p>
                </div>
            </div>
        `;

        document.getElementById('invariant-modal').style.display = 'block';
    }

    /**
     * Create help tooltips for UI elements
     */
    addHelpTooltips() {
        const tooltips = {
            'confidence-slider': 'Rate how sure you are about your answer (1=guessing, 5=certain)',
            'question-select': 'Choose which AP Statistics question to answer',
            'answer-input': 'Enter your answer (will be hashed for MCQs to prevent early influence)',
            'events-processed': 'Total number of attestations and interactions processed',
            'verification-list': 'Real-time system health checks ensuring all invariants are maintained'
        };

        Object.entries(tooltips).forEach(([id, text]) => {
            const element = document.getElementById(id);
            if (element) {
                element.title = text;
                element.addEventListener('mouseover', (e) => this.showTooltip(e, text));
            }
        });
    }

    /**
     * Show tooltip on hover
     */
    showTooltip(event, text) {
        const tooltip = document.createElement('div');
        tooltip.className = 'custom-tooltip';
        tooltip.textContent = text;
        tooltip.style.left = event.pageX + 10 + 'px';
        tooltip.style.top = event.pageY + 10 + 'px';
        document.body.appendChild(tooltip);

        event.target.addEventListener('mouseleave', () => {
            tooltip.remove();
        }, { once: true });
    }

    /**
     * Create convergence visualization chart
     */
    createConvergenceChart(canvasId, data) {
        const canvas = document.getElementById(canvasId);
        if (!canvas) return;

        const ctx = canvas.getContext('2d');
        
        // Simple line chart implementation
        const width = canvas.width;
        const height = canvas.height;
        const padding = 40;
        
        ctx.clearRect(0, 0, width, height);
        
        // Draw axes
        ctx.strokeStyle = '#4CAF50';
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.moveTo(padding, height - padding);
        ctx.lineTo(width - padding, height - padding);
        ctx.moveTo(padding, height - padding);
        ctx.lineTo(padding, padding);
        ctx.stroke();
        
        // Draw data points
        if (data && data.length > 1) {
            ctx.strokeStyle = '#2196F3';
            ctx.lineWidth = 3;
            ctx.beginPath();
            
            data.forEach((point, index) => {
                const x = padding + (index / (data.length - 1)) * (width - 2 * padding);
                const y = height - padding - (point / 100) * (height - 2 * padding);
                
                if (index === 0) {
                    ctx.moveTo(x, y);
                } else {
                    ctx.lineTo(x, y);
                }
            });
            ctx.stroke();
            
            // Draw points
            ctx.fillStyle = '#FF9800';
            data.forEach((point, index) => {
                const x = padding + (index / (data.length - 1)) * (width - 2 * padding);
                const y = height - padding - (point / 100) * (height - 2 * padding);
                
                ctx.beginPath();
                ctx.arc(x, y, 4, 0, 2 * Math.PI);
                ctx.fill();
            });
        }
        
        // Labels
        ctx.fillStyle = '#ffffff';
        ctx.font = '12px Courier New';
        ctx.fillText('Time', width / 2 - 15, height - 10);
        ctx.save();
        ctx.translate(15, height / 2);
        ctx.rotate(-Math.PI / 2);
        ctx.fillText('Convergence %', -30, 0);
        ctx.restore();
    }

    /**
     * Create distribution histogram
     */
    createDistributionChart(canvasId, choices, counts) {
        const canvas = document.getElementById(canvasId);
        if (!canvas) return;

        const ctx = canvas.getContext('2d');
        const width = canvas.width;
        const height = canvas.height;
        const padding = 40;
        
        ctx.clearRect(0, 0, width, height);
        
        if (!choices || !counts || choices.length === 0) return;
        
        const maxCount = Math.max(...counts);
        const barWidth = (width - 2 * padding) / choices.length - 10;
        
        choices.forEach((choice, index) => {
            const barHeight = (counts[index] / maxCount) * (height - 2 * padding);
            const x = padding + index * (barWidth + 10);
            const y = height - padding - barHeight;
            
            // Draw bar
            ctx.fillStyle = '#4CAF50';
            ctx.fillRect(x, y, barWidth, barHeight);
            
            // Draw label
            ctx.fillStyle = '#ffffff';
            ctx.font = '14px Courier New';
            ctx.textAlign = 'center';
            ctx.fillText(choice, x + barWidth / 2, height - padding + 20);
            ctx.fillText(counts[index].toString(), x + barWidth / 2, y - 5);
        });
    }

    /**
     * Start interactive tutorial
     */
    startTutorial() {
        this.tutorialStep = 0;
        this.showTutorialStep();
    }

    /**
     * Show tutorial step overlay
     */
    showTutorialStep() {
        const steps = [
            {
                target: '.header',
                title: 'Welcome to AP Statistics PoK Blockchain!',
                content: 'This system teaches statistics through peer consensus. Let\'s take a tour!'
            },
            {
                target: '.atom-counter',
                title: 'System Architecture',
                content: 'Our system has 58 mathematical atoms across 6 subsystems, all verified by 13 invariants.'
            },
            {
                target: '#question-select',
                title: 'Select a Question',
                content: 'Choose an AP Statistics question to answer. Each question tests a specific concept.'
            },
            {
                target: '#answer-input',
                title: 'Submit Your Answer',
                content: 'Enter your answer. For multiple choice, it will be encrypted until consensus forms.'
            },
            {
                target: '#confidence-slider',
                title: 'Set Your Confidence',
                content: 'How sure are you? Higher confidence gives more reputation if you\'re right, but costs more if wrong.'
            },
            {
                target: '#view-display',
                title: 'View System State',
                content: 'See the current view and system status. Navigate between different views using the buttons above.'
            }
        ];

        if (this.tutorialStep >= steps.length) {
            this.endTutorial();
            return;
        }

        const step = steps[this.tutorialStep];
        this.showTutorialOverlay(step);
    }

    /**
     * Show tutorial overlay for specific element
     */
    showTutorialOverlay(step) {
        // Remove existing overlay
        const existing = document.getElementById('tutorial-overlay');
        if (existing) existing.remove();

        const overlay = document.createElement('div');
        overlay.id = 'tutorial-overlay';
        overlay.className = 'tutorial-overlay';
        overlay.innerHTML = `
            <div class="tutorial-backdrop"></div>
            <div class="tutorial-popup">
                <h3>${step.title}</h3>
                <p>${step.content}</p>
                <div class="tutorial-controls">
                    <button onclick="skipTutorial()">Skip Tour</button>
                    <button onclick="nextTutorialStep()" class="primary">Next (${this.tutorialStep + 1}/${6})</button>
                </div>
            </div>
        `;

        document.body.appendChild(overlay);

        // Highlight target element
        const target = document.querySelector(step.target);
        if (target) {
            target.classList.add('tutorial-highlight');
            setTimeout(() => target.classList.remove('tutorial-highlight'), 3000);
        }
    }

    /**
     * End tutorial
     */
    endTutorial() {
        const overlay = document.getElementById('tutorial-overlay');
        if (overlay) overlay.remove();
        
        this.showSuccessMessage('Tutorial completed! You\'re ready to start making attestations.');
    }

    /**
     * Show success message
     */
    showSuccessMessage(message) {
        const messageArea = document.getElementById('message-area');
        if (messageArea) {
            messageArea.className = 'message-area success';
            messageArea.textContent = `✅ ${message}`;
        }
    }

    /**
     * Initialize all educational features
     */
    initialize() {
        this.addHelpTooltips();
        
        // Add invariant help buttons throughout the UI
        this.addInvariantHelpers();
        
        // Start tutorial if it's the user's first visit
        if (!localStorage.getItem('tutorial-completed')) {
            setTimeout(() => this.startTutorial(), 2000);
        }
    }

    /**
     * Add help buttons for invariants throughout the UI
     */
    addInvariantHelpers() {
        const locations = [
            { selector: '#events-processed', invariant: 6, position: 'right' },
            { selector: '#confidence-slider', invariant: 3, position: 'left' },
            { selector: '#verification-list', invariant: 12, position: 'top' },
            { selector: '.nav-button', invariant: 13, position: 'bottom' }
        ];

        locations.forEach(loc => {
            const elements = document.querySelectorAll(loc.selector);
            elements.forEach((element, index) => {
                if (index === 0) { // Only add to first instance
                    const helpBtn = document.createElement('button');
                    helpBtn.className = 'invariant-help-btn';
                    helpBtn.innerHTML = '?';
                    helpBtn.title = `Learn about Invariant ${loc.invariant}`;
                    helpBtn.onclick = () => this.showInvariant(loc.invariant);
                    
                    element.style.position = 'relative';
                    element.appendChild(helpBtn);
                }
            });
        });
    }
}

// Global functions for tutorial control
window.nextTutorialStep = function() {
    if (window.educationalFeatures) {
        window.educationalFeatures.tutorialStep++;
        window.educationalFeatures.showTutorialStep();
    }
};

window.skipTutorial = function() {
    if (window.educationalFeatures) {
        window.educationalFeatures.endTutorial();
        localStorage.setItem('tutorial-completed', 'true');
    }
};

window.closeInvariantModal = function() {
    document.getElementById('invariant-modal').style.display = 'none';
};

window.showTab = function(tabName) {
    // Hide all tab contents
    document.querySelectorAll('.tab-content').forEach(tab => {
        tab.classList.remove('active');
    });
    document.querySelectorAll('.tab-button').forEach(btn => {
        btn.classList.remove('active');
    });

    // Show selected tab
    document.getElementById(tabName + '-tab').classList.add('active');
    event.target.classList.add('active');
};

// Initialize when DOM is ready
document.addEventListener('DOMContentLoaded', function() {
    window.educationalFeatures = new EducationalFeatures();
    window.educationalFeatures.initialize();
});